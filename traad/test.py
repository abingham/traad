# This is for supporting testing only.

import threading
import time

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
    """An arbitrary long-running task that lets us reproducibly test our support
    for polling such tasks.

    """
    lrt = LRThread(state, message)
    lrthreads.add(lrt)
    lrt.start()
