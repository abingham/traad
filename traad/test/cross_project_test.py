import unittest

from traad.rope.change_signature import (normalize_arguments,
                                         remove_argument)
from traad.state import State
from traad.test import common


class CrossTests(unittest.TestCase):
    def setUp(self):
        self.state = State()
        self.task_id = 1
        self.state.create(self.task_id)
        self.task_state = self.state.get_task_state(self.task_id)
        self.proj = common.activate_project({
            'main': ['basic'],
            'cross': ['use_bar'],
        })

    def tearDown(self):
        common.deactivate()

    def test_cross_normalize_arguments(self):
        normalize_arguments(
            self.proj,
            self.task_state,
            'basic/bar.py',
            163)

        common.compare_projects(
            'cross_basic_normalize_arguments',
            'main',
            'basic')

        common.compare_projects(
            'cross_use_bar_normalize_arguments',
            'cross',
            'use_bar')

    def test_cross_remove_argument(self):
        remove_argument(
            self.proj,
            self.task_state,
            1,
            'basic/bar.py',
            163)

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
