import os
import shutil
import unittest

from ..rope.interface import RopeInterface

THIS_DIR = os.path.split(__file__)[0]
ACTIVE_DIR = os.path.join(THIS_DIR, 'active')
PROJECT_DIR = os.path.join(THIS_DIR, 'projects')

class CrossTests(unittest.TestCase):
    def setUp(self):
        try:
            shutil.rmtree(ACTIVE_DIR)
        except OSError:
            pass

        os.mkdir(ACTIVE_DIR)

        for dirname in ['main', 'cross']:
            shutil.copytree(
                os.path.join(PROJECT_DIR, dirname),
                os.path.join(ACTIVE_DIR, dirname))

        self.ri = RopeInterface(
            os.path.join(ACTIVE_DIR, 'main'),
            cross_project_dirs = [os.path.join(ACTIVE_DIR, 'cross')])

    def tearDown(self):
        shutil.rmtree(ACTIVE_DIR)

    def test_cross_rename(self):
        self.ri.rename(
            'NotBar',
            'basic/bar.py',
            30)

    def test_cross_normalize_arguments(self):
        self.ri.normalize_arguments(
            'basic/bar.py',
            163)

    def test_cross_remove_argument(self):
        self.ri.remove_argument(
            1,
            'basic/bar.py',
            163)

if __name__ == '__main__':
    unittest.main()
