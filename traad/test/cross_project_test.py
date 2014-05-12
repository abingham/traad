import unittest

from traad.state import State
from traad.test import common


class CrossTests(unittest.TestCase):
    def setUp(self):
        self.state = State.start().proxy()
        self.task_id = 1
        self.state.create(self.task_id).get()
        self.task_state = self.state.get_task_state(self.task_id).get()
        self.proj = common.activate_project({
            'main': ['basic'],
            'cross': ['use_bar'],
        })

    def tearDown(self):
        self.proj.stop()
        self.state.stop()
        common.deactivate()

    def test_cross_normalize_arguments(self):
        self.proj.normalize_arguments(
            self.task_state,
            'basic/bar.py',
            163).get()

        common.compare_projects(
            'cross_basic_normalize_arguments',
            'main',
            'basic')

        common.compare_projects(
            'cross_use_bar_normalize_arguments',
            'cross',
            'use_bar')

    def test_cross_remove_argument(self):
        self.proj.remove_argument(
            self.task_state,
            1,
            'basic/bar.py',
            163).get()

        common.compare_projects(
            'cross_basic_remove_argument',
            'main',
            'basic')

        common.compare_projects(
            'cross_use_bar_remove_argument',
            'cross',
            'use_bar')

if __name__ == '__main__':
    unittest.main()
