import json
import subprocess
import time
import unittest

try:
    from urllib.error import URLError
    from urllib.request import Request, urlopen
except ImportError:
    from urllib2 import Request, URLError, urlopen

from traad.test import common


def json_request(url, data=None, method='POST'):
    if data is None:
        req = Request(
            url=url)
    else:
        req = Request(
            url=url,
            data=json.dumps(data).encode('utf-8'),
            headers={'Content-Type': 'application/json; charset=utf-8'})
    rsp = urlopen(req)
    return json.loads(rsp.read().decode('utf-8'))


def wait_for_server(host, port, timeout=5):
    """Wait up to ``timeout`` seconds for server to start.
    """
    start_time = time.time()
    while time.time() - start_time < timeout:
        try:
            json_request(
                url='http://{}:{}/root'.format(
                    host,
                    port),
                method='GET')
            return
        except URLError:
            time.sleep(0.01)

    raise OSError('Unable to start server!')


class JSONAPITests(unittest.TestCase):
    def setUp(self):
        common.activate({'main': ['basic']})
        self.host = 'localhost'
        self.port = '9752'
        self.server_proc = subprocess.Popen(
            ['python',
             '-m', 'traad.server',
             '-p', str(self.port),
             common.activated_path('main')])

        wait_for_server(self.host, self.port)

    def tearDown(self):
        self.server_proc.terminate()
        self.server_proc.wait()

    def test_rename(self):
        rsp_data = json_request(
            url='http://{}:{}/refactor/rename'.format(
                self.host, self.port),
            data={
                'name': 'Llama',
                'path': 'basic/foo.py',
                'offset': 8,
            })

        self.assertEqual(rsp_data['result'], 'ok')
        task_id = rsp_data['task_id']

        while True:
            rsp_data = json_request(
                url='http://{}:{}/task/{}'.format(
                    self.host,
                    self.port,
                    task_id),
                method='GET')

            if rsp_data['status'] == 'success':
                break
            self.assertNotEqual(rsp_data['status'], 'failure')

            time.sleep(0.01)

        common.compare_projects(
            'basic_rename_llama',
            'main',
            'basic')

if __name__ == '__main__':
    unittest.main()
