import unittest

from traad.rope.rename import rename
from traad.state import State
from traad.test import common


class Tests(unittest.TestCase):
    def setUp(self):
        self.proj = common.activate(['rename'])

    def tearDown(self):
        common.deactivate()

    def test_simple(self):
        state = State()
        state.create(1)
        task_state = state.get_task_state(1)
        rename(self.proj, task_state, 'Llama', 'basic/foo.py', 8)

if __name__ == '__main__':
    unittest.main()
