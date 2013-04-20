import os
import shutil
import unittest

from ..rope.interface import RopeInterface

THIS_DIR = os.path.split(__file__)[0]
ACTIVE_DIR = os.path.join(THIS_DIR, 'active')
PROJECT_DIR = os.path.join(THIS_DIR, 'projects')

class Tests(unittest.TestCase):
    def setUp(self):
        try:
            shutil.rmtree(ACTIVE_DIR)
        except OSError:
            pass

        os.mkdir(ACTIVE_DIR)

        shutil.copytree(
            os.path.join(PROJECT_DIR, 'rename'),
            os.path.join(ACTIVE_DIR, 'rename'))

        self.ri = RopeInterface(
            os.path.join(ACTIVE_DIR, 'rename'))

    def tearDown(self):
        shutil.rmtree(ACTIVE_DIR)

    def test_simple(self):
        rslt = self.ri.rename(
            'Llama',
            'basic/foo.py',
            8)
        self.assertEqual(len(rslt['files']), 2)

if __name__ == '__main__':
    unittest.main()
