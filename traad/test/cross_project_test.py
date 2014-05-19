import unittest

import with_fixture

from traad.state import State
from traad.test import common


class CrossTests(with_fixture.TestCase):
    def withFixture(self):
        with common.use_project({'main': ['basic'],
                                 'cross': ['use_bar']}) as self.proj,\
            common.use_proxy(State.start().proxy()) as self.state:

            self.task_id = 1
            self.state.create(self.task_id).get()
            self.task_state = self.state.get_task_state(self.task_id).get()

            yield

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
