import traad.trace
from traad.rope.validate import validate

import rope.refactor.importutils


class ImportUtilFunctions:
    """The import related functions of the rope interface.

    A base for RopeInterface.

    """

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
