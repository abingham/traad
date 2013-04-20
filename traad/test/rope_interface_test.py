import os
import unittest

from traad.rope.interface import RopeInterface
from traad.test import common


class Tests(unittest.TestCase):
    def setUp(self):
        self.ri = common.activate(['main'])

    def tearDown(self):
        common.deactivate()

    def test_find_occurences(self):
        # Find occurrences of the Foo class
        occ = self.ri.find_occurrences(
            8,
            'basic/foo.py')
        self.assertEqual(len(occ), 3)

    def test_find_implementations(self):
        impls = self.ri.find_implementations(
            33,
            'basic/overrides.py')
        self.assertEqual(len(impls), 1)

    def test_find_definition(self):
        this_dir = os.path.split(__file__)[0]
        with open(os.path.join(this_dir, 'projects', 'main', 'basic', 'bar.py'), 'r') as f:
            code = f.read()
        loc = self.ri.find_definition(code, 142, os.path.join('basic', 'bar.py'))
        self.assertEqual(
            loc,
            (os.path.join('basic', 'bar.py'),
             (91, 100), 91, False, 7))

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

    def test_undo_exceptions(self):
        self.assertRaises(
            IndexError,
            self.ri.undo)

        rslt = self.ri.rename(
            'Llama',
            'basic/foo.py',
            8)

        self.ri.undo()

        self.assertRaises(
            IndexError,
            self.ri.undo,
            1)

    def test_undo_adds_history(self):
        self.assertEqual(len(self.ri.proj.history.undo_list), 0)
        self.ri.rename(
            'Llama',
            'basic/foo.py',
            8)
        self.assertEqual(len(self.ri.proj.history.undo_list), 1)

    def test_redo_adds_history(self):
        self.ri.rename(
            'Llama',
            'basic/foo.py',
            8)
        self.assertEqual(len(self.ri.proj.history.redo_list), 0)
        self.ri.undo()
        self.assertEqual(len(self.ri.proj.history.redo_list), 1)

    def test_redo_exceptions(self):
        self.assertRaises(
            IndexError,
            self.ri.redo)

        self.ri.rename(
            'Llama',
            'basic/foo.py',
            8)

        self.ri.undo()
        self.ri.redo()

        self.assertRaises(
            IndexError,
            self.ri.redo,
            1)

    def test_undo_history(self):
        self.assertEqual(
            len(self.ri.undo_history()), 0)
        self.ri.rename(
            'Llama',
            'basic/foo.py',
            8)
        self.assertEqual(
            len(self.ri.undo_history()), 1)

    def test_undo_info(self):
        self.ri.rename(
            'Llama',
            'basic/foo.py',
            8)
        i = self.ri.undo_info(0)
        for k in ['description', 'time', 'full_change', 'changes']:
            self.assertIn(k, i)

    def test_undo_info_exceptions(self):
        self.assertRaises(
            IndexError,
            self.ri.undo_info,
            0)
        self.ri.rename(
            'Llama',
            'basic/foo.py',
            8)
        self.ri.undo_info(0)
        self.assertRaises(
            IndexError,
            self.ri.undo_info,
            1)

    def test_redo_history(self):
        self.assertEqual(
            len(self.ri.redo_history()), 0)
        self.ri.rename(
            'Llama',
            'basic/foo.py',
            8)
        self.ri.undo()
        self.assertEqual(
            len(self.ri.redo_history()), 1)

    def test_redo_info(self):
        self.ri.rename(
            'Llama',
            'basic/foo.py',
            8)
        self.ri.undo()
        i = self.ri.redo_info(0)
        for k in ['description', 'time', 'full_change', 'changes']:
            self.assertIn(k, i)

    def test_redo_info_exceptions(self):
        self.assertRaises(
            IndexError,
            self.ri.redo_info,
            0)
        self.ri.rename(
            'Llama',
            'basic/foo.py',
            8)
        self.ri.undo()
        self.ri.redo_info(0)
        self.assertRaises(
            IndexError,
            self.ri.redo_info,
            1)

if __name__ == '__main__':
    unittest.main()
