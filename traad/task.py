import sys


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

            task_state.update({
                'task_info': str(self),
                'status': 'started',
            })

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

    def __repr__(self):
        return 'AsyncTask(task_id={}, func={}, args={}'.format(
            self.task_id,
            self.func,
            self.args)

    def __str__(self):
        return repr(self)
