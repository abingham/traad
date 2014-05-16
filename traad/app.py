import contextlib
import itertools
import logging
import sys

from . import bottle
from .rope.project import Project
from .state import State, TaskState


log = logging.getLogger('traad.app')

class ProjectApp(bottle.Bottle):
    def __init__(self):
        super(ProjectApp, self).__init__()
        self.project = None
        self.state = None
        self.task_ids = itertools.count()

app = ProjectApp()

@contextlib.contextmanager
def bind_to_project(project_path):
    app.project = Project.start(project_path).proxy()
    app.state = State.start().proxy()

    try:
        yield app
    finally:
        app.state.stop()
        app.project.stop()

@app.get('/root')
def project_root_view():
    try:
        return {
            'result': 'success',
            'root': bottle.request.app.project.get_root().get()
        }
    except e:
        return 'huh...'

@app.get('/all_resources')
def all_resources():
    return {
        'result': 'success',
        'resources': bottle.request.app.project.get_all_resources().get()
    }

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


@app.post('/history/undo')
def undo_view():
    args = bottle.request.json
    bottle.request.app.project.undo(args['index']).get()

    # TODO: What if it actually fails?
    return {'result': 'success'}


@app.post('/history/redo')
def redo_view():
    args = bottle.request.json
    bottle.request.app.project.redo(args['index']).get()

    # TODO: What if it actually fails?
    return {'result': 'success'}


@app.get('/history/view_undo')
def undo_history_view():
    return {
        'result': 'success',
        'history': bottle.request.app.project.undo_history().get()
    }


@app.get('/history/view_redo')
def redo_history_view():
    return {
        'result': 'success',
        'history': bottle.request.app.project.redo_history().get()
    }


@app.get('/history/undo_info/<idx>')
def undo_info_view(idx):
    return {
        'result': 'success',
        'info': bottle.request.app.project.undo_info(int(idx)).get()
    }


@app.get('/history/redo_info/<idx>')
def redo_info_view(idx):
    return {
        'result': 'success',
        'info': bottle.request.app.project.redo_info(int(idx)).get()
    }


@app.post('/test/long_running')
def long_running_test():
    import traad.test.tasks as tasks
    args = bottle.request.json

    return standard_async_task(tasks.long_running,
                               args['message'])

@app.post('/refactor/rename')
def rename_view():
    args = bottle.request.json
    return standard_async_task(bottle.request.app.project.rename,
                               args['name'],
                               args['path'],
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
    return extract_core(bottle.request.app.project.extract_method, bottle.request)


@app.post('/refactor/extract_variable')
def extract_variable_view():
    return extract_core(bottle.request.app.project.extract_variable, bottle.request)


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

    results = bottle.request.app.project.code_assist(args['code'],
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

    doc = bottle.request.app.project.get_doc(
        code=args['code'],
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

    calltip = bottle.request.app.project.get_calltip(
        code=args['code'],
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
    data = bottle.request.app.project.find_definition(
        args['code'],
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
    return _importutil_core(bottle.request, bottle.request.app.project.organize_imports)


@app.post("/imports/expand_star")
def expand_star_imports_view():
    return _importutil_core(bottle.request, bottle.request.app.project.expand_star_imports)


@app.post("/imports/froms_to_imports")
def from_to_imports_view():
    return _importutil_core(bottle.request, bottle.request.app.project.froms_to_imports)


@app.post("/imports/relatives_to_absolutes")
def relatives_to_absolutes_view():
    return _importutil_core(bottle.request, bottle.request.app.project.relatives_to_absolutes)


@app.post("/imports/handle_long_imports")
def handle_long_imports_view():
    return _importutil_core(bottle.request, bottle.request.app.project.handle_long_imports)


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
