import json
import subprocess
import time

try:
    from urllib.request import Request, urlopen
except ImportError:
    from urllib2 import Request, urlopen


def make_req(loc, data=None):
    if data is not None:
        data = json.dumps(data).encode('utf-8')
    req = Request(
        url='http://localhost:6543{}'.format(loc),
        data=data,
        headers={'Content-Type': 'application/json'})
    f = urlopen(req)
    return f.read()


def run_test(proc):
    data = make_req('/test/long_running',
                    {'message': 'Test message!'})
    task_id = json.loads(data.decode('utf-8'))['task_id']

    results = set()

    start_time = time.time()
    while time.time() - start_time < 10:
        results.add(
            make_req('/task/{}'.format(task_id)))
        time.sleep(0.1)

    assert len(results) == 10


try:
    proc = subprocess.Popen(
            ['python', '-m', 'traad.server', '-p', '6543', '.'])
    time.sleep(1)
    run_test(proc)
finally:
    proc.terminate()
    proc.wait()
