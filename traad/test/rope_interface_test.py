import os
import unittest

from traad.test import common


class Tests(unittest.TestCase):
    def setUp(self):
        self.ri = common.activate(['main'])

    def tearDown(self):
        common.deactivate()

    def test_get_all_resources(self):
        self.assertEqual(
            sorted(self.ri.get_all_resources()),
            [('', True),
             ('basic', True),
             ('basic/__init__.py', False),
             ('basic/bar.py', False),
             ('basic/foo.py', False),
             ('basic/overrides.py', False)])

    def test_get_children(self):
        self.assertEqual(
            sorted(self.ri.get_children('basic')),
            [('basic/__init__.py', False),
             ('basic/bar.py', False),
             ('basic/foo.py', False),
             ('basic/overrides.py', False)])



if __name__ == '__main__':
    unittest.main()
