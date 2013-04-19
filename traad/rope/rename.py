from rope.refactor import rename

import traad.trace
from traad.rope.validate import validate


class RenameFunctions:
    """The rename related functions of the rope interface.

    A base for RopeInterface.

    """

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

        ref = self.multi_project_refactoring(
            rename.Rename,
            self.proj,
            self.proj.get_resource(path),
            offset)

        # We want to return the list of changed files
        files = [res.real_path for res in ref.ref.get_changes(new_name).get_changed_resources()]

        ref.perform(new_name)

        return { 'files': files }
