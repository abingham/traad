import multiprocessing
import os
import shutil
import time
import unittest

from ..server import ProjectServer
from ..xmlrpc import ServerProxy

proj_dir = 'test_project'

proj_files = {
    'a.py': '''
class Foo:
    pass
''',

    'b.py': '''
from a import Foo
'''
}

server_host = 'localhost'
server_port = 1975

def run_server():
    server = ProjectServer(
        proj_dir,
        (server_host, server_port))
    server.serve_forever()


class Tests(unittest.TestCase):
    def setUp(self):
        try:
            shutil.rmtree(proj_dir)
        except OSError:
            pass

        os.mkdir(proj_dir)

        for name,data in proj_files.items():
            with open(os.path.join(proj_dir, name), 'w') as f:
                f.write(data)

        self.p = multiprocessing.Process(
            target=run_server)
        self.p.start()

        # TODO: Devise better way to know that server is
        # serving. Perhaps a simple poll.
        time.sleep(0.1)

    def tearDown(self):
        self.p.terminate()
        shutil.rmtree(proj_dir)

    def test_startup(self):
        pass

    def test_get_all_resources(self):
        s = ServerProxy(
            'http://{}:{}'.format(
                server_host, server_port))
        self.assertEqual(
            sorted(s.get_all_resources()),
            [['', True], ['a.py', False], ['b.py', False]])

if __name__ == '__main__':
    unittest.main()