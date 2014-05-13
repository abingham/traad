import json
import os
import subprocess
import sys
import time
import unittest

import requests

from traad.test import common


request_type_map = {
    'GET': requests.get,
    'POST': requests.post,
    }

def json_request(url, data=None, method='GET'):
    req_type = request_type_map[method]
    req = req_type(url,
                   data=json.dumps(data),
                   headers={'Content-Type': 'application/json; charset=utf-8'})
    return req.json()


def wait_for_server(host, port, timeout=5):
    """Wait up to ``timeout`` seconds for server to start.
    """
    start_time = time.time()
    while time.time() - start_time < timeout:
        try:
            json_request(
                url='http://{}:{}/root'.format(
                    host,
                    port))
            return
        except requests.exceptions.ConnectionError:
            time.sleep(0.01)

    raise OSError('Unable to start server!')


def wait_for_task(task_id, host, port):
    while True:
        rsp_data = json_request(
            url='http://{}:{}/task/{}'.format(
                host,
                port,
                task_id))

        if rsp_data['status'] == 'success':
            return True
        elif rsp_data['status'] == 'failure':
            return False

        time.sleep(0.01)


@unittest.skipUnless(sys.version_info.major == 3,
                     'Only run for Python version 3+')
class JSONAPITests(unittest.TestCase):
    def setUp(self):
        common.activate({'main': ['basic']})
        self.host = 'localhost'
        self.port = '9752'
        self.server_proc = subprocess.Popen(
            ['python',
             '-m', 'traad.server',
             '-p', str(self.port),
             common.activated_path('main')],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL)

        wait_for_server(self.host, self.port)

    def tearDown(self):
        self.server_proc.terminate()
        self.server_proc.wait()
        common.deactivate()

    def test_rename(self):
        rsp_data = json_request(
            url='http://{}:{}/refactor/rename'.format(
                self.host, self.port),
            data={
                'name': 'Llama',
                'path': 'basic/foo.py',
                'offset': 8,
            })

        self.assertEqual(rsp_data['result'], 'success')
        task_id = rsp_data['task_id']

        self.assertTrue(
            wait_for_task(task_id, self.host, self.port))

        common.compare_projects(
            'basic_rename_llama',
            'main',
            'basic')

    def test_find_occurrences(self):
        rsp_data = json_request(
            url='http://{}:{}/findit/occurrences'.format(
                self.host, self.port),
            data={
                'path': 'basic/foo.py',
                'offset': 8,
            })
        self.assertEqual(len(rsp_data['data']), 3)

    def test_find_implementations(self):
        rsp_data = json_request(
            url='http://{}:{}/findit/implementations'.format(
                self.host, self.port),
            data={
                'path': 'basic/overrides.py',
                'offset': 33,
            })
        self.assertEqual(len(rsp_data['data']), 1)

    def test_find_definition(self):
        path = os.path.join(
            common.activated_path('main'),
            'basic', 'bar.py')
        with open(path, 'r') as f:
            code = f.read()

        rsp_data = json_request(
            url='http://{}:{}/findit/definition'.format(
                self.host, self.port),
            data={
                'code': code,
                'path': 'basic/bar.py',
                'offset': 142,
            })

        self.assertEqual(
            rsp_data['data'],
            [os.path.join('basic', 'bar.py'),
             [91, 100],
             91,
             False,
             7])

    def test_undo_undoes_changes(self):
        rsp_data = json_request(
            url='http://{}:{}/refactor/rename'.format(
                self.host, self.port),
            data={
                'name': 'Llama',
                'path': 'basic/foo.py',
                'offset': 8,
            })

        self.assertEqual(rsp_data['result'], 'success')
        task_id = rsp_data['task_id']

        self.assertTrue(
            wait_for_task(task_id, self.host, self.port))

        with self.assertRaises(ValueError):
            common.compare_projects(
                'basic',
                'main',
                'basic')

        rsp_data = json_request(
            url='http://{}:{}/history/undo'.format(
                self.host, self.port),
            data={'index': 0})

        self.assertEqual(rsp_data['result'], 'success')

        common.compare_projects(
            'basic',
            'main',
            'basic')

if __name__ == '__main__':
    unittest.main()
