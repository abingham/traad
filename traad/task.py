class AsyncTask:
    def __init__(self, proj, state, task_id, func, *args):
        self.proj = proj
        self.state = state
        self.task_id = task_id
        self.func = func
        self.args = args

    def __call__(self):
        task_state = self.state.get_task_state(self.task_id)
        with self.proj.lock():
            self.func(self.proj,
                      task_state,
                      *self.args)
