import unittest

from traad.state import State, TaskState
from traad.test import common


class Tests(unittest.TestCase):
    def setUp(self):
        self.proj = common.activate_project({'main': ['basic']})
        self.state = State.start().proxy()
        self.state.create(1).get()
        self.task_state = TaskState(self.state, 1)

    def tearDown(self):
        self.proj.stop()
        self.state.stop()
        common.deactivate()

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
