import unittest

from traad.rope import history
from traad.rope.rename import rename
from traad.state import State
from traad.test import common


class HistoryTests(unittest.TestCase):
    def setUp(self):
        self.proj = common.activate_project({
            'main': ['basic'],
        })
        state = State()
        state.create(1)
        self.task_state = state.get_task_state(1)

    def tearDown(self):
        common.deactivate()

    def test_undo_undoes_changes(self):
        rename(self.proj, self.task_state,
               'Llama',
               'basic/foo.py',
               8)

        with self.assertRaises(ValueError):
            common.compare_projects(
                'basic',
                'main',
                'basic')

        history.undo(self.proj)

        common.compare_projects(
            'basic',
            'main',
            'basic')

    def test_undo_exceptions(self):
        with self.assertRaises(IndexError):
            history.undo(self.proj)

        rename(self.proj, self.task_state,
               'Llama',
               'basic/foo.py',
               8)

        history.undo(self.proj)

        with self.assertRaises(IndexError):
            history.undo(self.proj, 1)

    def test_undo_adds_history(self):
        self.assertEqual(len(self.proj.proj.history.undo_list), 0)
        rename(self.proj, self.task_state,
               'Llama',
               'basic/foo.py',
               8)
        self.assertEqual(len(self.proj.proj.history.undo_list), 1)

    def test_redo_redoes_changes(self):
        rename(self.proj, self.task_state,
               'Llama',
               'basic/foo.py',
               8)

        with self.assertRaises(ValueError):
            common.compare_projects(
                'basic',
                'main',
                'basic')

        history.undo(self.proj)

        common.compare_projects(
            'basic',
            'main',
            'basic')

        history.redo(self.proj)

        with self.assertRaises(ValueError):
            common.compare_projects(
                'basic',
                'main',
                'basic')

    def test_redo_adds_history(self):
        rename(self.proj, self.task_state,
               'Llama',
               'basic/foo.py',
               8)
        self.assertEqual(len(self.proj.proj.history.redo_list), 0)
        self.assertEqual(len(self.proj.proj.history.undo_list), 1)
        history.undo(self.proj)
        self.assertEqual(len(self.proj.proj.history.redo_list), 1)
        self.assertEqual(len(self.proj.proj.history.undo_list), 0)
        history.redo(self.proj)
        self.assertEqual(len(self.proj.proj.history.redo_list), 0)
        self.assertEqual(len(self.proj.proj.history.undo_list), 1)

    def test_redo_exceptions(self):
        with self.assertRaises(IndexError):
            history.redo(self.proj)

        rename(self.proj, self.task_state,
               'Llama',
               'basic/foo.py',
               8)

        history.undo(self.proj)
        history.redo(self.proj)

        with self.assertRaises(IndexError):
            history.redo(self.proj, 1)

    def test_undo_history(self):
        self.assertEqual(
            len(history.undo_history(self.proj)), 0)
        rename(self.proj, self.task_state,
               'Llama',
               'basic/foo.py',
               8)
        self.assertEqual(
            len(history.undo_history(self.proj)), 1)

    def test_undo_info(self):
        rename(self.proj, self.task_state,
               'Llama',
               'basic/foo.py',
               8)
        i = history.undo_info(self.proj, 0)
        for k in ['description', 'time', 'full_change', 'changes']:
            self.assertIn(k, i)

    def test_undo_info_exceptions(self):
        with self.assertRaises(IndexError):
            history.undo_info(self.proj, 0)

        rename(self.proj, self.task_state,
               'Llama',
               'basic/foo.py',
               8)
        history.undo_info(self.proj, 0)
        with self.assertRaises(IndexError):
            history.undo_info(self.proj, 1)

    def test_redo_history(self):
        self.assertEqual(
            len(history.redo_history(self.proj)), 0)
        rename(self.proj, self.task_state,
               'Llama',
               'basic/foo.py',
               8)
        history.undo(self.proj)
        self.assertEqual(
            len(history.redo_history(self.proj)), 1)

    def test_redo_info(self):
        rename(self.proj, self.task_state,
               'Llama',
               'basic/foo.py',
               8)
        history.undo(self.proj)
        i = history.redo_info(self.proj, 0)
        for k in ['description', 'time', 'full_change', 'changes']:
            self.assertIn(k, i)

    def test_redo_info_exceptions(self):
        with self.assertRaises(IndexError):
            history.redo_info(self.proj, 0)

        rename(self.proj, self.task_state,
               'Llama',
               'basic/foo.py',
               8)
        history.undo(self.proj)

        history.redo_info(self.proj, 0)

        with self.assertRaises(IndexError):
            history.redo_info(self.proj, 1)

if __name__ == '__main__':
    import logging
    logging.basicConfig(level=logging.CRITICAL)
    unittest.main()
