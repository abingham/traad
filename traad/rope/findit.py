from eagertools import emap
import rope.contrib.findit

import traad.trace
from traad.rope.validate import validate


def location_to_tuple(location):
    return (location.resource.path,
            location.region,
            location.offset,
            location.unsure,
            location.lineno)

class FinditFunctions:
    """The findit related functions of the rope interface.

    A base for RopeInterface.

    """

    def _find_locations(self, func, path, offset):
        """Common implementation for occurrences and
        implementations.

        """
        path = self._to_relative_path(path)
        results = func(
            self.proj,
            self.proj.get_resource(path),
            offset)
        return emap(location_to_tuple, results)

    @traad.trace.trace
    @validate
    def find_occurrences(self, offset, path):
        """Find occurrences of a symbol at a point in a file.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          offset: The offset into ``path`` of the symbol.
          path: The path to the resource containing the symbol to
            search for.

        Returns: A sequence of tuples of the form (path, (region-start, region-stop),
          offset, unsure, lineno).
        """

        return self._find_locations(
            rope.contrib.findit.find_occurrences,
            path, offset)

    @traad.trace.trace
    @validate
    def find_implementations(self, offset, path):
        """Find the places a given method is overridden.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          offset: The offset into ``path`` of the method name.
          path: The path to the resource containing the method name to
            search for.

        Returns: A sequence of tuples of the form (path, (region-start, region-stop),
          offset, unsure, lineno).
        """
        return self._find_locations(
            rope.contrib.findit.find_implementations,
            path, offset)
