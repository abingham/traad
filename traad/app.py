import contextlib
import itertools
import logging
import os.path
import sys

from . import bottle
from .rope.project import Project
from .state import State, TaskState


log = logging.getLogger('traad.app')

PROTOCOL_VERSION = 2


class ProjectApp(bottle.Bottle):
    def __init__(self):
        super(ProjectApp, self).__init__()
        self.projects = {}
        self.state = None
        self.task_ids = itertools.count()

    def find_project(self, path):
        """Get the active project for a `path`, if any.

        If there is no active project for `path`, raise `KeyError`.
        """
        for project_path, project in self.projects.items():
            common = os.path.commonprefix([project_path, path])
            if os.path.samefile(common, project_path):
                return project
        raise LookupError(
            'No existing project for path {}'.format(path))

    def create_project(self, path):
        """Create a new project rooted at `path`.
        """
        log.info('Creating new project at {}'.format(path))
        return self.projects.setdefault(path, Project.start(path).proxy())

app = ProjectApp()


@contextlib.contextmanager
def activate_app():
    """Initialize the global app's state actor and yield the app.

    When this context manager exits, it stop the state actor.
    """
    app.state = State.start().proxy()

    try:
        yield app
    finally:
        app.state.stop()


@app.get('/protocol_version')
def protocol_version_view():
    return {'protocol-version': PROTOCOL_VERSION}


@app.get('/root')
def project_root_view():
    """Get the root directory of the active project for `args['path']`.
    """
    args = bottle.request.json
    path = args['path']

    try:
        project = bottle.request.app.find_project(path)
        return {
            'result': 'success',
            'root': project.get_root().get()
        }
    except KeyError:
        bottle.abort(
            404,
            'No active project instance for {}'.format(path))


# @app.get('/all_resources')
# def all_resources():
#     return {
#         'result': 'success',
#         'resources': bottle.request.app.project.get_all_resources().get()
#     }


@app.get('/task/<task_id>')
def task_status_view(task_id):
    try:
        return bottle.request.app.state.get_task_state(int(task_id)).get()
    except KeyError:
        bottle.abort(404, "No task with that ID")


@app.get('/tasks')
def full_task_status():
    status = bottle.request.app.state.get_full_state().get()
    log.info('full status: {}'.format(status))
    return status


# TODO: We need to think about how to manage history when there are
# multiple projects. History is specific to a single project, so users
# will need to be able to distinguish between them and specify which
# they mean. Hmmm...
@app.post('/history/undo')
def undo_view():
    args = bottle.request.json
    root = args['root']
    index = args['index']
    bottle.request.app.projects[root].undo(index).get()

    # TODO: What if it actually fails?
    return {'result': 'success'}


@app.post('/history/redo')
def redo_view():
    args = bottle.request.json
    root = args['root']
    index = args['index']
    bottle.request.app.projects[root].redo(index).get()

    # TODO: What if it actually fails?
    return {'result': 'success'}


@app.get('/history/view_undo')
def undo_history_view():
    data = {path: project.undo_history().get()
            for path, project
            in bottle.request.app.projects.items()}

    return {
        'result': 'success',
        'history': data
    }


@app.get('/history/view_redo')
def redo_history_view():
    data = {path: project.redo_history().get()
            for path, project
            in bottle.request.app.projects.items()}

    return {
        'result': 'success',
        'history': data
    }


# @app.get('/history/undo_info/<idx>')
# def undo_info_view(idx):
#     return {
#         'result': 'success',
#         'info': bottle.request.app.project.undo_info(int(idx)).get()
#     }


# @app.get('/history/redo_info/<idx>')
# def redo_info_view(idx):
#     return {
#         'result': 'success',
#         'info': bottle.request.app.project.redo_info(int(idx)).get()
#     }


@app.post('/test/long_running')
def long_running_test():
    import traad.test.tasks as tasks
    args = bottle.request.json

    return standard_async_task(tasks.long_running,
                               args['message'])


def parent_dirs(path):
    last = None
    parent = path
    while parent != last:
        last = parent
        parent = os.path.dirname(last)
        yield parent


def find_project_root(path):
    for parent_dir in parent_dirs(path):
        if os.path.exists(os.path.join(parent_dir, '.ropeproject')):
            return parent_dir

    raise LookupError(
        'No parent directory of {} contains .ropeproject'.format(path))


def get_project(app, path):
    try:
        return app.find_project(path)
    except LookupError:
        try:
            root_dir = find_project_root(path)
        except LookupError:
            raise bottle.HTTPError(
                500,
                data={'exception':
                      {'type': 'LookupError',
                       'message': 'Unable to find suitable project root for {}'.format(path)}})

        return app.create_project(root_dir)


@app.post('/refactor/rename')
def rename_view():
    args = bottle.request.json
    path = args['path']
    project = get_project(bottle.request.app, path)

    return standard_async_task(project.rename,
                               args['name'],
                               path,
                               args.get('offset'))


def extract_core(method, request):
    """Common implementation for extract-method and extract-variable views.

    Args:
      method: The refactoring method to actually call.
      request: The bottle request for the refactoring.
    """
    args = request.json
    return standard_async_task(method,
                               args['name'],
                               args['path'],
                               args['start-offset'],
                               args['end-offset'])


@app.post('/refactor/extract_method')
def extract_method_view():
    return extract_core(bottle.request.app.project.extract_method,
                        bottle.request)


@app.post('/refactor/extract_variable')
def extract_variable_view():
    return extract_core(bottle.request.app.project.extract_variable,
                        bottle.request)


@app.post('/refactor/normalize_arguments')
def normalize_arguments_view():
    args = bottle.request.json
    return standard_async_task(bottle.request.app.project.normalize_arguments,
                               args['path'],
                               args['offset'])


@app.post('/refactor/remove_argument')
def remove_argument_view():
    args = bottle.request.json
    return standard_async_task(bottle.request.app.project.remove_argument,
                               args['arg_index'],
                               args['path'],
                               args['offset'])


@app.post('/code_assist/completions')
def code_assist_completion_view():
    args = bottle.request.json

    log.info('get completion: {}'.format(args))

    with open(args['path'], 'r') as f:
        code = f.read()

    results = bottle.request.app.project.code_assist(code,
                                                     args['offset'],
                                                     args['path']).get()

    # TODO: What if it fails?
    return {
        'result': 'success',
        'completions': results,
    }


@app.post('/code_assist/doc')
def code_assist_doc_view():
    args = bottle.request.json

    log.info('get doc: {}'.format(args))

    with open(args['path'], 'r') as f:
        code = f.read()

    doc = bottle.request.app.project.get_doc(
        code=code,
        offset=args['offset'],
        path=args['path']).get()

    return {
        'result': 'failure' if doc is None else 'success',
        'doc': doc
    }


@app.post('/code_assist/calltip')
def code_assist_calltip_view():
    args = bottle.request.json

    log.info('get calltip: {}'.format(args))

    with open(args['path'], 'r') as f:
        code = f.read()

    calltip = bottle.request.app.project.get_calltip(
        code=code,
        offset=args['offset'],
        path=args['path']).get()

    return {
        'result': 'failure' if calltip is None else 'success',
        'calltip': calltip
    }


# @get('/code_assist/definition')
# def code_assist_definition_view():
#     args = request.json

#     log.info('get definition: {}'.format(args))

#     return {
#         'results': bottle.request.app.project.get_definition_location(
#             code=args['code'],
#             offset=args['offset'],
#             path=args['path'])
#     }


@app.post('/findit/occurrences')
def findit_occurences_view():
    args = bottle.request.json
    data = bottle.request.app.project.find_occurrences(
        args['offset'],
        args['path']).get()

    # TODO: What if it actually fails?
    return {
        'result': 'success',
        'data': data,
    }


@app.post('/findit/implementations')
def findit_implementations_view():
    args = bottle.request.json
    data = bottle.request.app.project.find_implementations(
        args['offset'],
        args['path']).get()

    # TODO: What if it actually fails?
    return {
        'result': 'success',
        'data': data,
    }


@app.post('/findit/definition')
def findit_definitions_view():
    args = bottle.request.json

    with open(args['path'], 'r') as f:
        code = f.read()

    data = bottle.request.app.project.find_definition(
        code,
        args['offset'],
        args['path']).get()

    # TODO: What if it actually fails?
    return {
        'result': 'success',
        'data': data,
    }


def _importutil_core(request, method):
    # TODO: This patterns of async-tasks is repeated several times.
    # Refactor it.

    args = request.json
    return standard_async_task(
        method,
        args['path'])


@app.post("/imports/organize")
def organize_imports_view():
    return _importutil_core(bottle.request,
                            bottle.request.app.project.organize_imports)


@app.post("/imports/expand_star")
def expand_star_imports_view():
    return _importutil_core(bottle.request,
                            bottle.request.app.project.expand_star_imports)


@app.post("/imports/froms_to_imports")
def from_to_imports_view():
    return _importutil_core(bottle.request,
                            bottle.request.app.project.froms_to_imports)


@app.post("/imports/relatives_to_absolutes")
def relatives_to_absolutes_view():
    return _importutil_core(bottle.request,
                            bottle.request.app.project.relatives_to_absolutes)


@app.post("/imports/handle_long_imports")
def handle_long_imports_view():
    return _importutil_core(bottle.request,
                            bottle.request.app.project.handle_long_imports)


def standard_async_task(method, *args):
    """Launch a typical async task.

    This creates a `TaskState` for the new task and runs the task. The
    assumption here is that `method` is a pykka actor method that will
    execute in a separate thread. This function doesn't do anything to
    magically make `method` execute asynchronously.

    Args:
      method: The asynchronous callable to execute.
      args: The arguments to pass to ``method``.

    """
    log.info('{}: {}'.format(method, args))

    try:
        task_id = next(bottle.request.app.task_ids)
        state = bottle.request.app.state
        state.create(task_id)

        method(TaskState(state, task_id),
               *args)

        log.info('{}: success'.format(method))

        return {
            'result': 'success',
            'task_id': task_id
        }
    except:
        e = sys.exc_info()[1]
        log.error('{} error: {}'.format(method, e))
        return {
            'result': 'failure',
            'message': str(e)
        }
