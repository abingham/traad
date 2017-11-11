import common
import paths
import pytest


@pytest.fixture
def workspace(activate_package, make_workspace):
    activate_package(package='basic', into='main')
    workspace = make_workspace('main')

    yield workspace


def test_undo_undoes_changes(workspace):
    workspace.perform(
        workspace.rename(
            'basic/foo.py',
            8,
            'Llama'))

    with pytest.raises(ValueError):
        common.compare_workspaces(
            paths.packages('basic'),
            paths.active('main', 'basic'))

    workspace.undo()

    common.compare_workspaces(
        paths.packages('basic'),
        paths.active('main', 'basic'))


def test_undo_exceptions(workspace):
    with pytest.raises(IndexError):
        workspace.undo()

    workspace.perform(
        workspace.rename(
            'basic/foo.py',
            8,
            'Llama'))

    workspace.undo()

    with pytest.raises(IndexError):
        workspace.undo(1)


def test_undo_adds_history(workspace):
    assert len(workspace.root_project.history.undo_list) == 0

    workspace.perform(
        workspace.rename(
            'basic/foo.py',
            8,
            'Llama'))

    assert len(workspace.root_project.history.undo_list) == 1


def test_redo_redoes_changes(workspace):
    workspace.perform(
        workspace.rename(
            'basic/foo.py',
            8,
            'Llama'))

    with pytest.raises(ValueError):
        common.compare_workspaces(
            paths.packages('basic'),
            paths.active('main', 'basic'))

    workspace.undo()

    common.compare_workspaces(
        paths.packages('basic'),
        paths.active('main', 'basic'))

    workspace.redo()

    with pytest.raises(ValueError):
        common.compare_workspaces(
            paths.packages('basic'),
            paths.active('main', 'basic'))


def test_redo_adds_history(workspace):
    workspace.perform(
        workspace.rename(
            'basic/foo.py',
            8,
            'Llama'))

    assert len(workspace.root_project.history.redo_list) == 0
    assert len(workspace.root_project.history.undo_list) == 1

    workspace.undo()

    assert len(workspace.root_project.history.redo_list) == 1
    assert len(workspace.root_project.history.undo_list) == 0

    workspace.redo()

    assert len(workspace.root_project.history.redo_list) == 0
    assert len(workspace.root_project.history.undo_list) == 1


def test_redo_exceptions(workspace):
    with pytest.raises(IndexError):
        workspace.redo()

    workspace.perform(
        workspace.rename(
            'basic/foo.py',
            8,
            'Llama'))

    workspace.undo()

    workspace.redo()

    with pytest.raises(IndexError):
        workspace.redo(1)


def test_undo_history(workspace):
    assert len(workspace.undo_history()) == 0
    workspace.perform(
        workspace.rename('basic/foo.py',
                         8,
                         'Llama'))
    assert len(workspace.undo_history()) == 1


def test_undo_info(workspace):
    workspace.perform(
        workspace.rename('basic/foo.py',
                         8,
                         'Llama'))
    i = workspace.undo_info(0)
    for k in ['description', 'time', 'full_change', 'changes']:
        assert k in i


def test_undo_info_exceptions(workspace):
    with pytest.raises(IndexError):
        workspace.undo_info(0)

    workspace.perform(
        workspace.rename('basic/foo.py',
                         8,
                         'Llama'))

    workspace.undo_info(0)

    with pytest.raises(IndexError):
        workspace.undo_info(1)


def test_redo_history(workspace):
    assert len(workspace.redo_history()) == 0

    workspace.perform(
        workspace.rename('basic/foo.py',
                         8,
                         'Llama'))

    workspace.undo()

    assert len(workspace.redo_history()) == 1


def test_redo_info(workspace):
    workspace.perform(
        workspace.rename('basic/foo.py',
                         8,
                         'Llama'))

    workspace.undo()

    i = workspace.redo_info(0)
    for k in ['description', 'time', 'full_change', 'changes']:
        assert k in i


def test_redo_info_exceptions(workspace):
    with pytest.raises(IndexError):
        workspace.redo_info(0)

    workspace.perform(
        workspace.rename('basic/foo.py',
                         8,
                         'Llama'))

    workspace.undo()

    workspace.redo_info(0)
