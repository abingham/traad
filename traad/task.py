class AsyncTask:
    def __init__(self, proj, state, task_id, func, *args):
        self.proj = proj
        self.state = state
        self.task_id = task_id
        self.func = func
        self.args = args

    def __call__(self):
        try:
            task_state = self.state.get_task_state(self.task_id)

            task_state.update({'status': 'started'})

            with self.proj.lock():
                self.func(self.proj,
                          task_state,
                          *self.args)

            task_state.update({
                'status': 'success',
            })
        except:
            task_state.update({
                'status': 'failure',
                'message': str(sys.exc_info()[1]),
            })
            raise
