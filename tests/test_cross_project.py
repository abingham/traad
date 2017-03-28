import common
import paths
import pytest
from traad.state import TaskState


@pytest.fixture
def fixture(activate_package, start_project, state):
    activate_package(package='basic', into='main')
    activate_package(package='use_bar', into='cross')
    proj = start_project('main', 'cross')
    state.create(1).get()
    task_state = TaskState(state, 1)

    yield proj, state, task_state


def test_cross_normalize_arguments(fixture):
    project, state, task_state = fixture

    project.normalize_arguments(
        task_state,
        'basic/bar.py',
        163).get()    # 163 offset = cursor position on "a_free_func"

    common.compare_projects(
        paths.approved('cross_basic_normalize_arguments'),
        paths.active('main', 'basic'))

    common.compare_projects(
        paths.approved('cross_use_bar_normalize_arguments'),
        paths.active('cross', 'use_bar'))


def test_cross_remove_argument(fixture):
    project, state, task_state = fixture

    project.remove_argument(
        task_state,
        1,
        'basic/bar.py',
        163).get()

    common.compare_projects(
        paths.approved('cross_basic_remove_argument'),
        paths.active('main', 'basic'))

    common.compare_projects(
        paths.approved('cross_use_bar_remove_argument'),
        paths.active('cross', 'use_bar'))


def test_cross_add_argument(self):
    project, state, task_state = fixture

    project.add_argument(
        task_state,
        'basic/bar.py',
        163,
        3,
        'newarg',
        None,
        "newvalue").get()

    common.compare_projects(
        paths.approved('cross_basic_add_argument'),
        paths.active('main', 'basic'))

