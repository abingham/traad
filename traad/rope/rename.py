import rope.refactor.rename

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

        renamer = rope.refactor.rename.Rename(
            self.proj,
            self.proj.get_resource(path),
            offset)

        self.proj.do(renamer.get_changes(new_name))
