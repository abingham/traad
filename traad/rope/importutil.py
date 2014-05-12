import traad.trace
from traad.rope.validate import validate

import rope.refactor.importutils


class ImportUtilsMixin:
    @validate
    def _importutil_func(self, state, path, funcname):
        # TODO: Update state in some useful way.
        path = self.to_relative_path(path)
        iorg = rope.refactor.importutils.ImportOrganizer(self.proj)
        changes = getattr(iorg, funcname)(self.proj.get_resource(path))
        if changes:
            self.proj.do(changes)


    @traad.trace.trace
    def organize_imports(self, state, path):
        """Organize the import statements in a python source file.

        Args:
        path: The path of the file to reorganize.
        """

        # TODO: This takes more arguments.

        return self._importutil_func(state, path,
                                     "organize_imports")


    @traad.trace.trace
    def expand_star_imports(self, state, path):
        """Expand "star" import statements in a python source file.

        Args:
        path: The path of the file to reorganize.
        """
        return self._importutil_func(state, path,
                                     "expand_star_imports")


    @traad.trace.trace
    def froms_to_imports(self, state, path):
        """Convert "from" imports to normal imports.

        Args:
        path: The path of the file to reorganize.
        """
        return self._importutil_func(state, path,
                                     "froms_to_imports")


    @traad.trace.trace
    def relatives_to_absolutes(self, state, path):
        """Convert relative imports to absolute.

        Args:
        path: The path of the file to reorganize.
        """
        return self._importutil_func(state, path,
                                     "relatives_to_absolutes")


    @traad.trace.trace
    def handle_long_imports(self, state, path):
        """Clean up long import statements.

        Args:
        path: The path of the file to reorganize.
        """
        return self._importutil_func(state, path,
                                     "handle_long_imports")
