import unittest

import with_fixture

from traad.state import State, TaskState
from traad.test import common


class Tests(with_fixture.TestCase):
    def withFixture(self):
        with common.use_project({'main': ['basic']}) as self.proj,\
             common.use_proxy(State.start().proxy()) as self.state:
            self.state.create(1).get()
            self.task_state = TaskState(self.state, 1)
            yield

    def test_simple(self):
        self.proj.rename(
            self.task_state,
            'Llama',
            'basic/foo.py',
            8).get()

        common.compare_projects(
            'basic_rename_llama',
            'main',
            'basic')

if __name__ == '__main__':
    unittest.main()
