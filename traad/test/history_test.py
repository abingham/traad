import unittest

import with_fixture

from traad.rope import history
from traad.state import State, TaskState
from traad.test import common


class HistoryTests(with_fixture.TestCase):
    def withFixture(self):
        with common.use_project({'main': ['basic']}) as self.proj,\
             common.use_proxy(State.start().proxy()) as self.state:
            self.state.create(1).get()
            self.task_state = TaskState(self.state, 1)
            yield

    def test_undo_undoes_changes(self):
        self.proj.rename(
            self.task_state,
            'Llama',
            'basic/foo.py',
            8).get()

        with self.assertRaises(ValueError):
            common.compare_projects(
                'basic',
                'main',
                'basic')

        self.proj.undo().get()

        common.compare_projects(
            'basic',
            'main',
            'basic')

    def test_undo_exceptions(self):
        with self.assertRaises(IndexError):
            self.proj.undo().get()

        self.proj.rename(
            self.task_state,
            'Llama',
            'basic/foo.py',
            8).get()

        self.proj.undo().get()

        with self.assertRaises(IndexError):
            self.proj.undo(1).get()

    def test_undo_adds_history(self):
        self.assertEqual(len(self.proj.proj.get().history.undo_list), 0)
        self.proj.rename(
            self.task_state,
            'Llama',
            'basic/foo.py',
            8).get()
        self.assertEqual(len(self.proj.proj.get().history.undo_list), 1)

    def test_redo_redoes_changes(self):
        self.proj.rename(
            self.task_state,
            'Llama',
            'basic/foo.py',
            8).get()

        with self.assertRaises(ValueError):
            common.compare_projects(
                'basic',
                'main',
                'basic')

        self.proj.undo().get()

        common.compare_projects(
            'basic',
            'main',
            'basic')

        self.proj.redo().get()

        with self.assertRaises(ValueError):
            common.compare_projects(
                'basic',
                'main',
                'basic')

    def test_redo_adds_history(self):
        self.proj.rename(
            self.task_state,
            'Llama',
            'basic/foo.py',
            8).get()
        self.assertEqual(len(self.proj.proj.get().history.redo_list), 0)
        self.assertEqual(len(self.proj.proj.get().history.undo_list), 1)
        self.proj.undo().get()
        self.assertEqual(len(self.proj.proj.get().history.redo_list), 1)
        self.assertEqual(len(self.proj.proj.get().history.undo_list), 0)
        self.proj.redo().get()
        self.assertEqual(len(self.proj.proj.get().history.redo_list), 0)
        self.assertEqual(len(self.proj.proj.get().history.undo_list), 1)

    def test_redo_exceptions(self):
        with self.assertRaises(IndexError):
            self.proj.redo().get()

        self.proj.rename(
            self.task_state,
            'Llama',
            'basic/foo.py',
            8).get()

        self.proj.undo().get()
        self.proj.redo().get()

        with self.assertRaises(IndexError):
            self.proj.redo(1).get()

    def test_undo_history(self):
        self.assertEqual(
            len(self.proj.undo_history().get()), 0)
        self.proj.rename(self.task_state,
               'Llama',
               'basic/foo.py',
               8).get()
        self.assertEqual(
            len(self.proj.undo_history().get()), 1)

    def test_undo_info(self):
        self.proj.rename(self.task_state,
               'Llama',
               'basic/foo.py',
               8).get()
        i = self.proj.undo_info(0).get()
        for k in ['description', 'time', 'full_change', 'changes']:
            self.assertIn(k, i)

    def test_undo_info_exceptions(self):
        with self.assertRaises(IndexError):
            self.proj.undo_info(0).get()

        self.proj.rename(self.task_state,
               'Llama',
               'basic/foo.py',
               8).get()
        self.proj.undo_info(0).get()
        with self.assertRaises(IndexError):
            self.proj.undo_info(1).get()

    def test_redo_history(self):
        self.assertEqual(
            len(self.proj.redo_history().get()), 0)
        self.proj.rename(self.task_state,
               'Llama',
               'basic/foo.py',
               8).get()
        self.proj.undo().get()
        self.assertEqual(
            len(self.proj.redo_history().get()), 1)

    def test_redo_info(self):
        self.proj.rename(self.task_state,
               'Llama',
               'basic/foo.py',
               8).get()
        self.proj.undo().get()
        i = self.proj.redo_info(0).get()
        for k in ['description', 'time', 'full_change', 'changes']:
            self.assertIn(k, i)

    def test_redo_info_exceptions(self):
        with self.assertRaises(IndexError):
            self.proj.redo_info(0).get()

        self.proj.rename(self.task_state,
               'Llama',
               'basic/foo.py',
               8).get()
        self.proj.undo().get()

        self.proj.redo_info(0)

        with self.assertRaises(IndexError):
            self.proj.redo_info(1).get()

if __name__ == '__main__':
    import logging
    logging.basicConfig(level=logging.CRITICAL)
    unittest.main()
