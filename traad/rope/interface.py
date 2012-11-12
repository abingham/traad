import os

import rope.base.project
import rope.contrib.codeassist
import rope.refactor.change_signature
import rope.refactor.importutils
import rope.refactor.rename

import traad.trace
from traad.rope.extract import ExtractFunctions
from traad.rope.history import HistoryFunctions
from traad.rope.log import log
from traad.rope.validate import validate


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

class RopeInterface(ExtractFunctions,
                    HistoryFunctions):
    def __init__(self,
                 project_dir):
        self.proj = rope.base.project.Project(project_dir)

    @traad.trace.trace
    @validate
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

    @traad.trace.trace
    def get_all_resources(self):
        '''Get a list of all resources in the project.

        Returns: A list of tuples of the form (path,
            is_folder).
        '''
        return list(get_all_resources(self.proj))

    @traad.trace.trace
    @validate
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

    @traad.trace.trace
    @validate
    def remove_argument(self, arg_index, path, offset):
        """Remove an argument from a method.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          arg_index: The index of the argument to remove.
          path: The path of the file/directory to query.
          offset: The offset in the resource of the method signature.
        """

        path = self._to_relative_path(path)

        change_sig = rope.refactor.change_signature.ChangeSignature(
            self.proj,
            self.proj.get_resource(path),
            offset)

        self.proj.do(
            change_sig.get_changes(
                [rope.refactor.change_signature.ArgumentRemover(arg_index)]))

    @traad.trace.trace
    @validate
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

    @traad.trace.trace
    @validate
    def get_doc(self, code, offset, path):
        '''Get docstring for an object.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          code: The source code.
          offset: An offset into ``code`` of the object to query.
          path: The path to the resource in which the completion is
            being done.

        Returns: The docstring for the object.
        '''

        path = self._to_relative_path(path)
        return rope.contrib.codeassist.get_doc(
            self.proj,
            code,
            offset,
            self.proj.get_resource(path))

    @traad.trace.trace
    @validate
    def get_definition_location(self, code, offset, path):
        '''Get docstring for an object.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          code: The source code.
          offset: An offset into ``code`` of the object to query.
          path: The path to the resource in which the search is
            being done.

        Returns: A tuple of the form (path, lineno). If no definition
          can be found, then (None, None) is returned.
        '''
        path = self._to_relative_path(path)
        rslt = rope.contrib.codeassist.get_definition_location(
            self.proj,
            code,
            offset,
            self.proj.get_resource(path))

        if rslt[1] is None:
            return rslt

        if rslt[0] is None:
            return (path, rslt[1])

        return (rslt[0].real_path, rslt[1])

    @validate
    def _importutil_func(self, path, funcname):
        path = self._to_relative_path(path)
        iorg = rope.refactor.importutils.ImportOrganizer(self.proj)
        changes = getattr(iorg, funcname)(self.proj.get_resource(path))
        if changes:
            self.proj.do(changes)

    @traad.trace.trace
    def organize_imports(self, path):
        """Organize the import statements in a python source file.

        Args:
          path: The path of the file to reorganize.
        """
        return self._importutil_func(path, "organize_imports")

    @traad.trace.trace
    def expand_star_imports(self, path):
        """Expand "star" import statements in a python source file.

        Args:
          path: The path of the file to reorganize.
        """
        return self._importutil_func(path, "expand_star_imports")

    @traad.trace.trace
    def froms_to_imports(self, path):
        """Convert "from" imports to normal imports.

        Args:
          path: The path of the file to reorganize.
        """
        return self._importutil_func(path, "froms_to_imports")

    @traad.trace.trace
    def relatives_to_absolutes(self, path):
        """Convert relative imports to absolute.

        Args:
          path: The path of the file to reorganize.
        """
        return self._importutil_func(path, "relatives_to_absolutes")

    @traad.trace.trace
    def handle_long_imports(self, path):
        """Clean up long import statements.

        Args:
          path: The path of the file to reorganize.
        """
        return self._importutil_func(path, "handle_long_imports")

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

    def __repr__(self):
        return 'RopeInterface("{}")'.format(
            self.proj.root.real_path)

    def __str__(self):
        return repr(self)