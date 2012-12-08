from eagertools import emap
import rope.contrib.findit

import traad.trace
from traad.rope.validate import validate


def location_to_tuple(location):
    return (location.resource,
            location.region,
            location.offset,
            location.unsure,
            location.linenoe)

class FinditFunctions:
    """The findit related functions of the rope interface.

    A base for RopeInterface.

    """

    @traad.trace.trace
    @validate
    def find_occurrences(self, offset, path):
        '''Find occurrences of a symbol at a point in a file.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          offset: The offset into ``path`` of the symbol.
          path: The path to the resource containing the symbol to
            search for.

        Returns: A sequence of tuples of the form (path, region,
          offset, unsure, lineno).
        '''

        path = self._to_relative_path(path)
        results = rope.contrib.findit.find_occurrences(
            self.proj,
            self.get_resource(path),
            offset)
        return emap(location_to_tuple, results)