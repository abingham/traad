from rope.refactor import multiproject, rename

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

        CrossRename = multiproject.MultiProjectRefactoring(
            rename.Rename,
            self.other_projs)

        rename = CrossRename(
            self.proj,
            self.proj.get_resource(path),
            offset)

        multiproject.perform(
            renamer.get_all_changes(new_name))
