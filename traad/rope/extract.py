import rope.refactor.extract

import traad.trace
from traad.rope.validate import validate


@traad.trace.trace
@validate
def extract_method(project, state, name, path, start_offset, end_offset):
    path = project.to_relative_path(path)

    ref = project.make_refactoring(
        rope.refactor.extract.ExtractMethod,
        project.get_resource(path),
        start_offset,
        end_offset)

    change = ref.get_change(name)

    state.update(
        {'description': list(change.descriptions),
         'changed_resources': [r.name for r in change.resources],
     })

    change.perform()

class ExtractFunctions:
    """The extract related functions of the rope interface.

    A base for RopeInterface.

    """

    @validate
    def _extract(self, path, name, start_offset, end_offset, cls):
        '''Core extract-* method, parameterized on the class of
        the extraction.
        '''

        path = self._to_relative_path(path)

        extractor = cls(
            self.proj,
            self.proj.get_resource(path),
            start_offset,
            end_offset)

        self.proj.do(extractor.get_changes(name))

    @traad.trace.trace
    def extract_variable(self, name, path, start_offset, end_offset):
        '''Extract a variable.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          new_name: The name for the new variable.
          path: The path of the resource containing the code.
          start_offset: The starting offset of the region to extract.
          end_offset: The end (one past the last character) of the
            region to extract.
        '''

        self._extract(path,
                      name,
                      start_offset,
                      end_offset,
                      rope.refactor.extract.ExtractVariable)
