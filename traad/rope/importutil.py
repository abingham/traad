import traad.trace
from traad.rope.validate import validate

import rope.refactor.importutils


@validate
def _importutil_func(project, state, path, funcname):
    # TODO: Update state in some useful way.
    path = project.to_relative_path(path)
    iorg = rope.refactor.importutils.ImportOrganizer(project.proj)
    changes = getattr(iorg, funcname)(project.proj.get_resource(path))
    if changes:
        project.proj.do(changes)

@traad.trace.trace
def organize_imports(project, state, path):
    """Organize the import statements in a python source file.

    Args:
      path: The path of the file to reorganize.
    """

    # TODO: This takes more arguments.

    return _importutil_func(project, state, path, "organize_imports")


class ImportUtilFunctions:
    """The import related functions of the rope interface.

    A base for RopeInterface.

    """



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
