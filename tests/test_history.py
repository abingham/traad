import common
import pytest
from traad.rope import history
from traad.state import State, TaskState


@pytest.fixture
def fixture(copy_project, start_project, state):
    copy_project('basic', 'main')
    proj = start_project('main')
    state.create(1).get()
    task_state = TaskState(state, 1)

    yield proj, state, task_state


def test_undo_undoes_changes(fixture):
    project, state, task_state = fixture

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


def test_undo_exceptions(fixture):
    project, state, task_state = fixture

    with pytest.raises(IndexError):
        project.undo().get()

    project.rename(
        task_state,
        'Llama',
        'basic/foo.py',
        8).get()

    project.undo().get()

    with pytest.raises(IndexError):
        project.undo(1).get()


def test_undo_adds_history(fixture):
    project, state, task_state = fixture
    assert len(project.proj.get().history.undo_list) == 0
    project.rename(
        task_state,
        'Llama',
        'basic/foo.py',
        8).get()
    assert len(project.proj.get().history.undo_list) == 1


def test_redo_redoes_changes(fixture):
    project, state, task_state = fixture
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

    project.redo().get()

    with pytest.raises(ValueError):
        common.compare_projects(
            'basic',
            'main',
            'basic')


def test_redo_adds_history(fixture):
    project, state, task_state = fixture
    project.rename(
        task_state,
        'Llama',
        'basic/foo.py',
        8).get()
    assert len(project.proj.get().history.redo_list) == 0
    assert len(project.proj.get().history.undo_list) == 1
    project.undo().get()
    assert len(project.proj.get().history.redo_list) == 1
    assert len(project.proj.get().history.undo_list) == 0
    project.redo().get()
    assert len(project.proj.get().history.redo_list) == 0
    assert len(project.proj.get().history.undo_list) == 1


def test_redo_exceptions(fixture):
    project, state, task_state = fixture
    with pytest.raises(IndexError):
        project.redo().get()

    project.rename(
        task_state,
        'Llama',
        'basic/foo.py',
        8).get()

    project.undo().get()
    project.redo().get()

    with pytest.raises(IndexError):
        project.redo(1).get()


def test_undo_history(fixture):
    project, state, task_state = fixture
    assert len(project.undo_history().get()) == 0
    project.rename(task_state,
                   'Llama',
                   'basic/foo.py',
                   8).get()
    assert len(project.undo_history().get()) == 1


def test_undo_info(fixture):
    project, state, task_state = fixture
    project.rename(task_state,
                   'Llama',
                   'basic/foo.py',
                   8).get()
    i = project.undo_info(0).get()
    for k in ['description', 'time', 'full_change', 'changes']:
        assert k in i


def test_undo_info_exceptions(fixture):
    project, state, task_state = fixture
    with pytest.raises(IndexError):
        project.undo_info(0).get()

    project.rename(task_state,
                   'Llama',
                   'basic/foo.py',
                   8).get()
    project.undo_info(0).get()
    with pytest.raises(IndexError):
        project.undo_info(1).get()


def test_redo_history(fixture):
    project, state, task_state = fixture
    assert len(project.redo_history().get()) == 0
    project.rename(task_state,
                   'Llama',
                   'basic/foo.py',
                   8).get()
    project.undo().get()
    assert len(project.redo_history().get()) == 1


def test_redo_info(fixture):
    project, state, task_state = fixture
    project.rename(task_state,
                   'Llama',
                   'basic/foo.py',
                   8).get()
    project.undo().get()
    i = project.redo_info(0).get()
    for k in ['description', 'time', 'full_change', 'changes']:
        assert k in i


def test_redo_info_exceptions(fixture):
    project, state, task_state = fixture
    with pytest.raises(IndexError):
        project.redo_info(0).get()

    project.rename(task_state,
                   'Llama',
                   'basic/foo.py',
                   8).get()
    project.undo().get()

    project.redo_info(0)
