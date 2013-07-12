from threading import Lock


class State:
    """Maps task-ids to dicts of data about the task.

    Most interaction with State is through TaskState objects which
    provide a task-id-specific interface.

    State is reentrant.
    """
    def __init__(self):
        self.data = {}
        self.lock = Lock()

    def create(self, task_id, initial_data=None):
        if initial_data is None:
            initial_data = {'status': 'pending'}

        with self.lock:
            assert task_id not in self.data
            self.data[task_id] = initial_data

    def get_task_state(self, task_id):
        return TaskState(self, task_id)

    def get_full_state(self):
        with self.lock:
            return dict(self.data)


class TaskState:
    def __init__(self, state, task_id):
        self.state = state
        self.task_id = task_id

    def get(self):
        with self.state.lock:
            return dict(self.state.data[self.task_id])

    def set(self, data):
        with self.state.lock:
            self.state.data[self.task_id] = dict(data)

    def update(self, data):
        with self.state.lock:
            d = dict(self.state.data[self.task_id])
            d.update(data)
            self.state.data[self.task_id] = d
