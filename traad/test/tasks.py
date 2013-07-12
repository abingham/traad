import time

from traad.rope.validate import validate
import traad.trace


@traad.trace.trace
@validate
def long_running(project, state, message):
    state.update({'status': 'starting'})

    time.sleep(1)

    for i in range(10):
        state.update({'status{}'.format(i): message})
        time.sleep(1)
