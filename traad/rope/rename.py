import sys

import rope.refactor

from traad.rope.validate import validate
import traad.trace


@traad.trace.trace
@validate
def rename(project, state, new_name, path, offset=None):
    path = project.to_relative_path(path)

    # Construct the refactoring object
    ref = project.make_refactoring(
        rope.refactor.rename.Rename,
        project.get_resource(path),
        offset)

    try:
        state.update({'status': 'started'})

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

        state.update({
            'status': 'success',
        })

    except:
        state.update({
            'status': 'failure',
            'message': str(sys.exc_info()[1]),
        })
        raise
