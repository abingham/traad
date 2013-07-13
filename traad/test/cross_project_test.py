import unittest

from traad.rope.change_signature import normalize_arguments
from traad.state import State
from traad.test import common


class CrossTests(unittest.TestCase):
    def setUp(self):
        self.state = State()
        self.task_id = 1
        self.state.create(self.task_id)
        self.task_state = self.state.get_task_state(self.task_id)
        self.proj = common.activate(['main', 'cross'])

    def tearDown(self):
        common.deactivate()

    def test_cross_normalize_arguments(self):
        normalize_arguments(
            self.proj,
            self.task_state,
            'basic/bar.py',
            163)

    @unittest.skip('reactivate')
    def test_cross_remove_argument(self):
        self.ri.remove_argument(
            1,
            'basic/bar.py',
            163)

if __name__ == '__main__':
    unittest.main()
