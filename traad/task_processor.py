import threading


class TaskProcessor(threading.Thread):
    def __init__(self, q, project, state):
        super(TaskProcessor, self).__init__()
        self.task_queue = q
        self.proj = project

    def run(self):
        while True:
            task = self.task_queue.get()

            try:
                # None means to quit.
                if task is None:
                    return

                task()
            finally:
                self.task_queue.task_done()
