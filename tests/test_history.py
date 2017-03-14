import common
import pytest
from traad.rope import history
from traad.state import State, TaskState


@pytest.fixture
def project(copy_project, start_project):
    copy_project('basic', 'main')
    yield start_project('main')


def test_undo_undoes_changes(project, state):
    state.create(1).get()
    task_state = TaskState(state, 1)

    project.rename(
        task_state,
        'Llama',
        'basic/foo.py',
        8).get()

    with pytest.raises(ValueError):
        common.compare_projects(
            'basic',
            'main',
            'basic')

    project.undo().get()

    common.compare_projects(
        'basic',
        'main',
        'basic')

# def test_undo_exceptions(fixture):
#     with pytest.raises(IndexError):
#         project.undo().get()

#     project.rename(
#         self.task_state,
#         'Llama',
#         'basic/foo.py',
#         8).get()

#     project.undo().get()

#     with pytest.raises(IndexError):
#         project.undo(1).get()

# def test_undo_adds_history(self):
#     self.assertEqual(len(project.project.get().history.undo_list), 0)
#     project.rename(
#         self.task_state,
#         'Llama',
#         'basic/foo.py',
#         8).get()
#     self.assertEqual(len(project.project.get().history.undo_list), 1)

# def test_redo_redoes_changes(self):
#     project.rename(
#         self.task_state,
#         'Llama',
#         'basic/foo.py',
#         8).get()

#     with pytest.raises(ValueError):
#         common.compare_projects(
#             'basic',
#             'main',
#             'basic')

#     project.undo().get()

#     common.compare_projects(
#         'basic',
#         'main',
#         'basic')

#     project.redo().get()

#     with pytest.raises(ValueError):
#         common.compare_projects(
#             'basic',
#             'main',
#             'basic')

# def test_redo_adds_history(self):
#     project.rename(
#         self.task_state,
#         'Llama',
#         'basic/foo.py',
#         8).get()
#     self.assertEqual(len(project.project.get().history.redo_list), 0)
#     self.assertEqual(len(project.project.get().history.undo_list), 1)
#     project.undo().get()
#     self.assertEqual(len(project.project.get().history.redo_list), 1)
#     self.assertEqual(len(project.project.get().history.undo_list), 0)
#     project.redo().get()
#     self.assertEqual(len(project.project.get().history.redo_list), 0)
#     self.assertEqual(len(project.project.get().history.undo_list), 1)

# def test_redo_exceptions(self):
#     with pytest.raises(IndexError):
#         project.redo().get()

#     project.rename(
#         self.task_state,
#         'Llama',
#         'basic/foo.py',
#         8).get()

#     project.undo().get()
#     project.redo().get()

#     with pytest.raises(IndexError):
#         project.redo(1).get()

# def test_undo_history(self):
#     self.assertEqual(
#         len(project.undo_history().get()), 0)
#     project.rename(self.task_state,
#             'Llama',
#             'basic/foo.py',
#             8).get()
#     self.assertEqual(
#         len(project.undo_history().get()), 1)

# def test_undo_info(self):
#     project.rename(self.task_state,
#             'Llama',
#             'basic/foo.py',
#             8).get()
#     i = project.undo_info(0).get()
#     for k in ['description', 'time', 'full_change', 'changes']:
#         self.assertIn(k, i)

# def test_undo_info_exceptions(self):
#     with pytest.raises(IndexError):
#         project.undo_info(0).get()

#     project.rename(self.task_state,
#             'Llama',
#             'basic/foo.py',
#             8).get()
#     project.undo_info(0).get()
#     with pytest.raises(IndexError):
#         project.undo_info(1).get()

# def test_redo_history(self):
#     self.assertEqual(
#         len(project.redo_history().get()), 0)
#     project.rename(self.task_state,
#             'Llama',
#             'basic/foo.py',
#             8).get()
#     project.undo().get()
#     self.assertEqual(
#         len(project.redo_history().get()), 1)

# def test_redo_info(self):
#     project.rename(self.task_state,
#             'Llama',
#             'basic/foo.py',
#             8).get()
#     project.undo().get()
#     i = project.redo_info(0).get()
#     for k in ['description', 'time', 'full_change', 'changes']:
#         self.assertIn(k, i)

# def test_redo_info_exceptions(self):
#     with pytest.raises(IndexError):
#         project.redo_info(0).get()

#     project.rename(self.task_state,
#             'Llama',
#             'basic/foo.py',
#             8).get()
#     project.undo().get()

#     project.redo_info(0)
