import multiprocessing
import os
import shutil
import time
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
            os.path.join(PROJECT_DIR, 'basic'),
            os.path.join(ACTIVE_DIR, 'basic'))

        self.ri = RopeInterface(
            os.path.join(ACTIVE_DIR))

    def tearDown(self):
        shutil.rmtree(ACTIVE_DIR)

    def test_find_occurences(self):
        # Find occurrences of the Foo class
        occ = self.ri.find_occurrences(
            8,
            'basic/foo.py')
        self.assertEqual(len(occ), 3)

    def test_get_all_resources(self):
        self.assertEqual(
            sorted(self.ri.get_all_resources()),
            [('', True),
             ('basic', True),
             ('basic/__init__.py', False),
             ('basic/bar.py', False),
             ('basic/foo.py', False)])

    def test_get_children(self):
        self.assertEqual(
            sorted(self.ri.get_children('basic')),
            [('basic/__init__.py', False),
             ('basic/bar.py', False),
             ('basic/foo.py', False)])

    def test_undo_exceptions(self):
        self.assertRaises(
            IndexError,
            self.ri.undo)

        self.ri.rename(
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