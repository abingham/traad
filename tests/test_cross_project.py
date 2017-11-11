import common
import paths
import pytest


@pytest.fixture
def workspace(activate_package, make_workspace):
    activate_package(package='basic', into='main')
    activate_package(package='use_bar', into='cross')
    workspace = make_workspace('main', 'cross')

    yield workspace


def test_cross_normalize_arguments(workspace):
    changes = workspace.normalize_arguments(
        'basic/bar.py',
        163)    # 163 offset = cursor position on "a_free_func"

    workspace.perform(changes)

    common.compare_workspaces(
        paths.approved('cross_basic_normalize_arguments'),
        paths.active('main', 'basic'))

    common.compare_workspaces(
        paths.approved('cross_use_bar_normalize_arguments'),
        paths.active('cross', 'use_bar'))


def test_cross_remove_argument(workspace):
    changes = workspace.remove_argument('basic/bar.py', 163, 1)

    workspace.perform(changes)

    common.compare_workspaces(
        paths.approved('cross_basic_remove_argument'),
        paths.active('main', 'basic'))

    common.compare_workspaces(
        paths.approved('cross_use_bar_remove_argument'),
        paths.active('cross', 'use_bar'))


def test_cross_add_argument(workspace):
    changes = workspace.add_argument(
        'basic/bar.py',
        163,
        3,
        'newarg',
        "\"default\"",
        "\"value\"")

    workspace.perform(changes)

    common.compare_workspaces(
        paths.approved('cross_basic_add_argument'),
        paths.active('main', 'basic'))
