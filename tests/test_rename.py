import common
import paths


def test_simple_rename(activate_package, make_workspace):
    activate_package(package='basic', into='main')
    workspace = make_workspace('main')

    changes = workspace.rename(
        'basic/foo.py',
        8,
        'Llama')

    workspace.perform(changes)

    common.compare_workspaces(
        paths.approved('basic_rename_llama'),
        paths.active('main', 'basic'))
