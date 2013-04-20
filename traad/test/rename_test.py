import unittest

from traad.rope.interface import RopeInterface
from traad.test import common

class Tests(unittest.TestCase):
    def setUp(self):
        self.ri = common.activate(['rename'])

    def tearDown(self):
        common.deactivate()

    def test_simple(self):
        rslt = self.ri.rename(
            'Llama',
            'basic/foo.py',
            8)
        self.assertEqual(len(rslt['files']), 2)

if __name__ == '__main__':
    unittest.main()
