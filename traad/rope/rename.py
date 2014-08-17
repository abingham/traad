import rope.refactor

from traad.rope.validate import validate
from traad.state import task_state_monitor
import traad.trace


class RenameMixin:
    @traad.trace.trace
    @validate
    @task_state_monitor
    def rename(self, state, new_name, path, offset=None):
        path = self.to_relative_path(path)

        # Construct the refactoring object
        ref = self.make_refactoring(
            rope.refactor.rename.Rename,
            self.get_resource(path),
            offset)

        # This gets a change object that knows about the new-name.
        change = ref.get_change(new_name)

        # Update some state. The state is reentrant.
        # Note that `state` encapsulates the task_id for this
        # refactoring, so we don't need to deal with it.
        state.update(
            {'description': list(change.descriptions),
             'changed_resources': [r.name for r in change.resources],
             })

        # actually run the refactoring
        change.perform()
