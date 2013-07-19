import unittest


class ProjectTests(unittest.TestCase):
    @unittest.skip('TODO')
    def test_get_all_resources(self):
        self.assertEqual(
            sorted(self.ri.get_all_resources()),
            [('', True),
             ('basic', True),
             ('basic/__init__.py', False),
             ('basic/bar.py', False),
             ('basic/foo.py', False),
             ('basic/overrides.py', False)])

    @unittest.skip('TODO')
    def test_get_children(self):
        self.assertEqual(
            sorted(self.ri.get_children('basic')),
            [('basic/__init__.py', False),
             ('basic/bar.py', False),
             ('basic/foo.py', False),
             ('basic/overrides.py', False)])
