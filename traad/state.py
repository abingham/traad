import sys

import decorator
import pykka


class State(pykka.ThreadingActor):
    """Maps task-ids to dicts of data about the task.

    This is essentially a dictionary of task-ids to dicts. Each dict
    describes the state of the task. `State` doesn't care about the
    contents of the dict.

    `State` is a pykka actor and thereby provides thread-save access
    to the task data.
    """
    def __init__(self):
        super(State, self).__init__()

        self.data = {}

    def create(self, task_id, initial_data=None):
        """Create a new entry in the state map for `task-id`.
        """
        if initial_data is None:
            initial_data = {'status': 'pending'}

        assert task_id not in self.data
        self.data[task_id] = initial_data

    def get_task_state(self, task_id):
        """Get the task state for `task-id`.
        """
        return self.data[task_id]

    def set_task_state(self, task_id, data):
        """Replace the task state for `task-id`.
        """
        self.data.task_id = dict(data)

    def update_task_state(self, task_id, data):
        """Update the task state for `task-id`.

        See dict.update() for details on what this means.
        """
        d = dict(self.data[task_id])
        d.update(data)
        self.data[task_id] = d

    def get_full_state(self):
        """Returns a complete copy of the current state.
        """
        # TODO: Should this use deepcopy? probably.
        return dict(self.data)

class TaskState:
    """A wrapper around `State` that provides access to just the state for
    particular task-id.

    """
    def __init__(self, state, task_id):
        self.state = state
        self.task_id = task_id

    def get(self):
        return self.state.get_task_state(self.task_id).get()

    def set(self, data):
        self.state.set_task_state(self.task_id, dict(data)).get()

    def update(self, data):
        return self.state.update_task_state(self.task_id, data).get()

@decorator.decorator
def task_state_monitor(f, self, task_state, *args, **kwargs):
    """A decorator that updates a `TaskState` to reflect success or
    failure of an operation.

    This executes `f`, passing it `self`, `task_state`, and all of the
    other arguments. If `f` runs without an exception, this updates
    `task_state` with {'status': 'success'} and returns `f`\'s return
    value. If `f` throws, this updates `task_state` with {'status':
    'failure', 'message': <exception info>} and reraises the
    exception.

    This is primarily intended for use on asynchronous calls that need
    to update the state based on their success or failure.

    """
    try:
        r = f(self, task_state, *args, **kwargs)
        task_state.update({'status': 'success'})
        return r
    except:
        task_state.update(
            {'status': 'failure',
             'message': str(sys.exc_info()[1]),
         })
        raise
