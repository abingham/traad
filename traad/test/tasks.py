import threading
import time

from traad.rope.validate import validate
import traad.trace


lrthreads = set()

class LRThread(threading.Thread):
    def __init__(self, state, message):
        super(LRThread, self).__init__()
        self.state = state
        self.message = message

    def run(self):
        self.state.update({'status': 'starting'})

        time.sleep(1)

        for i in range(10):
            self.state.update({'status{}'.format(i): self.message})
            time.sleep(1)

        lrthreads.discard(self)

@traad.trace.trace
def long_running(state, message):
    lrt = LRThread(state, message)
    lrthreads.add(lrt)
    lrt.start()
