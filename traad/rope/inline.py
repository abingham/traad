import rope.refactor.inline

from traad.rope.validate import validate
from traad.state import task_state_monitor
import traad.trace


class InlineMixin:
    @traad.trace.trace
    @validate
    @task_state_monitor
    def inline(self, state, path, offset):
        path = self.to_relative_path(path)
        ref = rope.refactor.inline.create_inline(
            self.proj,
            self.get_resource(path),
            offset)
        change_set = ref.get_changes()

        state.update(
            {'description':
             change_set.get_description(),
             'changed_resources':
             [r.name for r
              in change_set.get_changed_resources()]
            })

        self.proj.do(change_set)
