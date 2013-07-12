import baker
import json
import subprocess
import time
import urllib.request


def make_req(loc, data={}, method='POST'):
        req = urllib.request.Request(
            url='http://localhost:6543{}'.format(loc),
            data=json.dumps(data).encode('utf-8'),
            headers={'Content-Type': 'application/json'},
            method=method)
        f = urllib.request.urlopen(req)
        return f.read()


def run_test(proc):
    data = make_req('/test/long_running',
                    {'message': 'Test message!'})
    task_id = json.loads(data.decode('utf-8'))['task_id']

    results = set()

    start_time = time.time()
    while time.time() - start_time < 10:
        results.add(
            make_req('/task/{}'.format(task_id), method='GET'))
        time.sleep(0.1)

    assert len(results) == 10

with subprocess.Popen(['python', '-m', 'traad.server', '-p', '6543', '.']) as proc:
    try:
        time.sleep(1)
        run_test(proc)
    finally:
        proc.terminate()
