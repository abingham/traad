import json
import subprocess
import time

import requests


def make_req(loc, data=None):
    if data is not None:
        data = json.dumps(data).encode('utf-8')
    req = requests.get('http://localhost:6543{}'.format(loc),
                       data=data,
                       headers={'Content-Type': 'application/json'})
    return req.text

def run_test(proc):
    data = make_req('/test/long_running',
                    {'message': 'Test message!'})
    task_id = json.loads(data)['task_id']

    results = set()

    start_time = time.time()
    while time.time() - start_time < 15:
        rslt = make_req('/task/{}'.format(task_id))
        print(rslt)
        results.add(rslt)
        time.sleep(0.1)

    assert len(results) == 11


try:
    proc = subprocess.Popen(
            ['python', '-m', 'traad.server', '-p', '6543', '.'])
    time.sleep(1)
    run_test(proc)
finally:
    proc.terminate()
    proc.wait()
