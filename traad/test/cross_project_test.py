import os
import shutil
import unittest

from traad.rope.interface import RopeInterface
from traad.test import common

class CrossTests(unittest.TestCase):
    def setUp(self):
        self.ri = common.activate(['main', 'cross'])

    def tearDown(self):
        common.deactivate()

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
