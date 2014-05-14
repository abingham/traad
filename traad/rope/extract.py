import rope.refactor.extract

from traad.rope.validate import validate
from traad.state import task_state_monitor
import traad.trace


class ExtractMixin:
    @traad.trace.trace
    @validate
    @task_state_monitor
    def _extract(self, state, method, name, path, start_offset, end_offset):
        """Shared implementation of extract_method and _variable.
        """
        path = self.to_relative_path(path)

        ref = self.make_refactoring(
            method,
            self.get_resource(path),
            start_offset,
            end_offset)

        change = ref.get_change(name)

        state.update(
            {'description': list(change.descriptions),
             'changed_resources': [r.name for r in change.resources],
         })

        change.perform()


    @traad.trace.trace
    def extract_method(self, state, name, path, start_offset, end_offset):
        """Extract a method.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          new_name: The name for the new method.
          path: The path of the resource containing the code.
          start_offset: The starting offset of the region to extract.
          end_offset: The end (one past the last character) of the
            region to extract.
        """
        self._extract(state,
                      rope.refactor.extract.ExtractMethod,
                      name,
                      path,
                      start_offset,
                      end_offset)


    @traad.trace.trace
    def extract_variable(self, state, name, path, start_offset, end_offset):
        """Extract a variable.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          new_name: The name for the new variable.
          path: The path of the resource containing the code.
          start_offset: The starting offset of the region to extract.
          end_offset: The end (one past the last character) of the
            region to extract.
        """
        self._extract(state,
                      rope.refactor.extract.ExtractVariable,
                      name,
                      path,
                      start_offset,
                      end_offset)
