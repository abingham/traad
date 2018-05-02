import common
import paths


def test_simple_move(activate_package, make_workspace):
    activate_package(package='basic', into='main')
    workspace = make_workspace('main')

    changes = workspace.move(
        'basic/bar.py',
        31,
        'basic/foo.py')

    workspace.perform(changes)

    common.compare_workspaces(
        paths.approved('simple_move'),
        paths.active('main', 'basic'))
