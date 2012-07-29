import os

import rope.base.project
import rope.contrib.codeassist
import rope.refactor.extract
import rope.refactor.rename


def get_all_resources(proj):
    '''Generate a sequence of (path, is_folder) tuples for all
    resources in a project.

    Args:
      proj: The rope Project to scan.

    Returns: An iterable over all resources in a Project, with a tuple
      (path, is_folder) for each.
    '''
    todo = ['']
    while todo:
        res_path = todo[0]
        todo = todo[1:]
        res = proj.get_resource(res_path)
        yield(res.path, res.is_folder())

        if res.is_folder():
            todo.extend((child.path for child in res.get_children()))


class RopeInterface:
    def __init__(self,
                 project_dir):
        self.proj = rope.base.project.Project(project_dir)

    def get_children(self, path):
        '''Get a list of all child resources of a given path.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          path: The path of the file/directory to query.

        Returns: A list of tuples of the form (path,
          is_folder).

        '''

        path = self._to_relative_path(path)
        children = self.proj.get_resource(path).get_children()
        return [(child.path, child.is_folder()) for child in children]

    def get_all_resources(self):
        '''Get a list of all resources in the project.

        Returns: A list of tuples of the form (path,
            is_folder).
        '''
        return list(get_all_resources(self.proj))

    def undo(self):
        '''Undo the last operation.
        '''
        self.proj.history.undo()

    def redo(self):
        '''Redo the last undone operation.
        '''
        self.proj.history.redo()

    def undo_history(self):
        '''Get a list of undo-able changes.

        Returns:
          A list of descriptions of undoable changes/refactorings.
        '''
        return [cs.description for cs in self.proj.history.undo_list]

    def undo_info(self, idx):
        '''Get information about a single undoable operation.

        Args:
          idx: An index in the undo history.

        Raises:
          IndexError: ``idx`` is out of range.

        Returns:
           A dict of information about the undoable change.
        '''
        return self._history_info(self.proj.history.undo_list, idx)

    def redo_info(self, idx):
        '''Get information about a single redoable operation.

        Args:
          idx: An index in the redo history.

        Raises:
          IndexError: ``idx`` is out of range.

        Returns:
           A dict of information about the redoable change.
        '''
        return self._history_info(self.proj.history.redo_list, idx)

    def _history_info(self, seq, idx):
        def contents(c):
            return {
                'resource': c.resource.path,
                'change': c.get_description(),
            }

        c = seq[idx]
        return {
            'description': c.description,
            'time': c.time,
            'full_change': c.get_description(),
            'changes': [contents(x) for x in c.changes],
            }

    def redo_history(self):
        '''Get a list of redo-able changes.

        Returns:
          A list of descriptions of redoable changes/refactorings.
        '''
        return [cs.description for cs in self.proj.history.redo_list]

    def _extract(self, path, name, start_offset, end_offset, cls):
        '''Core extract-* method, parameterized on the class of
        the extraction.
        '''

        path = self._to_relative_path(path)

        extractor = cls(
            self.proj,
            self.proj.get_resource(path),
            start_offset,
            end_offset)

        self.proj.do(extractor.get_changes(name))

    def extract_method(self, name, path, start_offset, end_offset):
        '''Extract a method.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          new_name: The name for the new method.
          path: The path of the resource containing the code.
          start_offset: The starting offset of the region to extract.
          end_offset: The end (one past the last character) of the
            region to extract.
        '''

        self._extract(path,
                      name,
                      start_offset,
                      end_offset,
                      rope.refactor.extract.ExtractMethod)

    def extract_variable(self, name, path, start_offset, end_offset):
        '''Extract a variable.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          new_name: The name for the new variable.
          path: The path of the resource containing the code.
          start_offset: The starting offset of the region to extract.
          end_offset: The end (one past the last character) of the
            region to extract.
        '''

        self._extract(path,
                      name,
                      start_offset,
                      end_offset,
                      rope.refactor.extract.ExtractVariable)

    def rename(self, new_name, path, offset=None):
        '''Rename a resource.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          path: The path of the file/directory to query.
        '''

        path = self._to_relative_path(path)

        renamer = rope.refactor.rename.Rename(
            self.proj,
            self.proj.get_resource(path),
            offset)
        self.proj.do(renamer.get_changes(new_name))

    def code_assist(self, code, offset, path):
        '''Get code-assist completions for a point in a file.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          code: The source code in which the completion should
            happen. Note that this may differ from the contents of the
            resource at ``path``.
          offset: The offset into ``code`` where the completion should
            happen.
          path: The path to the resource in which the completion is
            being done.

        Returns: A list of tuples of the form (name, documentation,
          scope, type) for each possible completion.
        '''

        path = self._to_relative_path(path)
        results = rope.contrib.codeassist.code_assist(
            self.proj,
            code,
            offset,
            self.proj.get_resource(path))
        return [(r.name, r.get_doc(), r.scope, r.type) for r in results]

    def _to_relative_path(self, path):
        '''Get a version of a path relative to the project root.

        If ``path`` is already relative, then it is unchanged. If
        ``path`` is absolute, then it is made relative to the project
        root.

        Args:
          path: The path to make relative.

        Returns: ``path`` relative to the project root.

        '''
        if os.path.isabs(path):
            path = os.path.relpath(
                path,
                self.proj.root.real_path)
        return path
