import unittest

from traad.rope.rename import rename
from traad.state import State
from traad.test import common


class Tests(unittest.TestCase):
    def setUp(self):
        self.proj = common.activate_project({'main': ['basic']})
        self.state = State()
        self.state.create(1)
        self.task_state = self.state.get_task_state(1)

    def tearDown(self):
        common.deactivate()

    def test_simple(self):
        rename(self.proj,
               self.task_state,
               'Llama',
               'basic/foo.py',
               8)

        common.compare_projects(
            'basic_rename_llama',
            'main',
            'basic')

if __name__ == '__main__':
    unittest.main()
