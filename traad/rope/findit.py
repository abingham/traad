import rope.contrib.findit

from .validate import validate


def location_to_tuple(location):
    return (location.resource.path,
            location.region,
            location.offset,
            location.unsure,
            location.lineno)


def location_to_dict(location):
    return {
        "path": location.resource.path,
        "name": location.resource.name,
        "realpath": location.resource.real_path,
        # "isfolder": location.resource.is_folder(),
        # "parent": location.resource.parent,
        # "exists": location.resource.exists(),
        "region": location.region,
        "offset": location.offset,
        "unsure": location.unsure,
        "lineno": location.lineno,
    }


class FinditMixin:
    @validate
    def find_occurrences(self, offset, path):
        """Find occurrences of a symbol at a point in a file.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          offset: The offset into ``path`` of the symbol.
          path: The path to the resource containing the symbol to
            search for.

        Returns: A list of dicts (one per location) with elements: {"path",
          "name", "realpath", "region", "offset", "unsure", "lineno"}, or None
          if no implementations could be found.

        """

        results = rope.contrib.findit.find_occurrences(
            self.root_project,
            self.get_resource(path),
            offset,
            unsure=True)
        return list(map(location_to_dict, results))

    @validate
    def find_implementations(self, offset, path):
        """Find the places a given method is overridden.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          offset: The offset into ``path`` of the method name.
          path: The path to the resource containing the method name to
            search for.

        Returns: A list of dicts (one per location) with elements: {"path",
          "name", "realpath", "region", "offset", "unsure", "lineno"}, or None
          if no implementations could be found.

        """

        results = rope.contrib.findit.find_implementations(
            self.root_project,
            self.get_resource(path),
            offset)
        return list(map(location_to_dict, results))

    @validate
    def find_definition(self, code, offset, path):
        """Find the definition location of a symbol.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          code: The source code containing the method symbol.
          offset: The offset into ``code`` of the symbol.
          path: The path to the resource containing ``code``.

        Returns: A dict with elements {"path", "name", "realpath", "region",
          "offset", "unsure", "lineno"}, or None if the definition can't be
          found.

        """

        path = self.to_relative_path(path)
        defn = rope.contrib.findit.find_definition(
            self.root_project,
            code,
            offset,
            self.get_resource(path))

        return None if defn is None else location_to_dict(defn)
