import common
import paths
from traad.state import TaskState


def test_simple_rename(activate_package, start_project, state):
    activate_package(package='basic', into='main')
    proj = start_project('main')
    state.create(1).get()
    task_state = TaskState(state, 1)

    proj.rename(
        task_state,
        'Llama',
        'basic/foo.py',
        8).get()

    common.compare_projects(
        paths.approved('basic_rename_llama'),
        paths.active('main', 'basic'))
