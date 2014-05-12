import pykka

class State(pykka.ThreadingActor):
    """Maps task-ids to dicts of data about the task.

    This should be run as an actor.
    """
    def __init__(self):
        super(State, self).__init__()

        self.data = {}

    def create(self, task_id, initial_data=None):
        if initial_data is None:
            initial_data = {'status': 'pending'}

        assert task_id not in self.data
        self.data[task_id] = initial_data

    def get_task_state(self, task_id):
        return self.data[task_id]

    def set_task_state(self, task_id, data):
        self.data.task_id = dict(data)

    def update_task_state(self, task_id, data):
        d = dict(self.data[task_id])
        d.update(data)
        self.data[task_id] = d

    def get_full_state(self):
        return dict(self.data)

class TaskState:
    def __init__(self, state, task_id):
        self.state = state
        self.task_id = task_id

    def get(self):
        return self.state.get_task_satte(self.task_id).get()

    def set(self, data):
        self.state.set_task_state(self.task_id, dict(data)).get()

    def update(self, data):
        return self.state.update_task_state(self.task_id, data).get()
