from contextlib import contextmanager
import logging
import sys

from . import bottle
from .plugin import TraadPlugin
from .state import TaskState


log = logging.getLogger('traad.app')

PROTOCOL_VERSION = 2

app = bottle.Bottle()


@contextmanager
def using_project(project_path, app=app):
    """Context-manager that attaches a TraadPlugin to `app` for the specified path.

    This plugin will be uninstalled at the end of the context.
    """
    traad_plugin = TraadPlugin(project_path)
    app.install(traad_plugin)
    try:
        yield app
    finally:
        app.uninstall(traad_plugin)


@app.get('/protocol_version')
def protocol_version_view():
    return {'protocol-version': PROTOCOL_VERSION}


@app.get('/root')
def project_root_view(context):
    try:
        return {
            'result': 'success',
            'root': context.project.get_root().get()
        }
    except Exception:
        return 'huh...'


@app.get('/all_resources')
def all_resources(context):
    return {
        'result': 'success',
        'resources': context.project.get_all_resources().get()
    }


@app.get('/task/<task_id>')
def task_status_view(task_id, context):
    try:
        return context.state.get_task_state(int(task_id)).get()
    except KeyError:
        bottle.abort(404, "No task with that ID")


@app.get('/tasks')
def full_task_status(context):
    status = context.state.get_full_state().get()
    log.info('full status: {}'.format(status))
    return status


@app.post('/history/undo')
def undo_view(context):
    args = bottle.request.json
    context.project.undo(args['index']).get()

    # TODO: What if it actually fails?
    return {'result': 'success'}


@app.post('/history/redo')
def redo_view(context):
    args = bottle.request.json
    context.project.redo(args['index']).get()

    # TODO: What if it actually fails?
    return {'result': 'success'}


@app.get('/history/view_undo')
def undo_history_view(context):
    return {
        'result': 'success',
        'history': context.project.undo_history().get()
    }


@app.get('/history/view_redo')
def redo_history_view(context):
    return {
        'result': 'success',
        'history': context.project.redo_history().get()
    }


@app.get('/history/undo_info/<idx>')
def undo_info_view(idx, context):
    return {
        'result': 'success',
        'info': context.project.undo_info(int(idx)).get()
    }


@app.get('/history/redo_info/<idx>')
def redo_info_view(idx, context):
    return {
        'result': 'success',
        'info': context.project.redo_info(int(idx)).get()
    }


@app.post('/test/long_running')
def long_running_test(context):
    import traad.test
    args = bottle.request.json

    return standard_async_task(
        context,
        traad.test.long_running,
        args['message'])


@app.post('/refactor/rename')
def rename_view(context):
    args = bottle.request.json
    return standard_async_task(
        context,
        context.project.rename,
        args['name'],
        args['path'],
        args.get('offset'))


def extract_core(context, method):
    """Common implementation for extract-method and extract-variable views.

    Args:
      method: The refactoring method to actually call.
      request: The bottle request for the refactoring.
    """
    args = bottle.request.json
    return standard_async_task(
        context,
        method,
        args['name'],
        args['path'],
        args['start-offset'],
        args['end-offset'])


@app.post('/refactor/extract_method')
def extract_method_view(context):
    return extract_core(
        context,
        context.project.extract_method)


@app.post('/refactor/extract_variable')
def extract_variable_view(context):
    return extract_core(
        context,
        context.project.extract_variable)


@app.post('/refactor/inline')
def inline_view():
    args = bottle.request.json
    return standard_async_task(bottle.request.app.project.inline,
                               args['path'],
                               args['offset'])


@app.post('/refactor/normalize_arguments')
def normalize_arguments_view(context):
    args = bottle.request.json
    return standard_async_task(
        context,
        context.project.normalize_arguments,
        args['path'],
        args['offset'])


@app.post('/refactor/remove_argument')
def remove_argument_view(context):
    args = bottle.request.json
    return standard_async_task(
        context,
        context.project.remove_argument,
        args['arg_index'],
        args['path'],
        args['offset'])


@app.post('/refactor/add_argument')
def add_argument_view(context):
    args = bottle.request.json
    return standard_async_task(
        context,
        context.project.add_argument,
        args['path'],
        args['offset'],
        args['index'],
        args['name'],
        args['default'],
        args['value'])


@app.post('/code_assist/completions')
def code_assist_completion_view(context):
    args = bottle.request.json

    log.info('get completion: {}'.format(args))

    with open(args['path'], 'r') as f:
        code = f.read()

    results = context.project.code_assist(
        code,
        args['offset'],
        args['path']).get()

    # TODO: What if it fails?
    return {
        'result': 'success',
        'completions': results,
    }


@app.post('/code_assist/doc')
def code_assist_doc_view(context):
    args = bottle.request.json

    log.info('get doc: {}'.format(args))

    with open(args['path'], 'r') as f:
        code = f.read()

    doc = context.project.get_doc(
        code=code,
        offset=args['offset'],
        path=args['path']).get()

    return {
        'result': 'failure' if doc is None else 'success',
        'doc': doc
    }


@app.post('/code_assist/calltip')
def code_assist_calltip_view(context):
    args = bottle.request.json

    log.info('get calltip: {}'.format(args))

    with open(args['path'], 'r') as f:
        code = f.read()

    calltip = context.project.get_calltip(
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
#         'results': context.project.get_definition_location(
#             code=args['code'],
#             offset=args['offset'],
#             path=args['path'])
#     }


@app.post('/findit/occurrences')
def findit_occurences_view(context):
    args = bottle.request.json
    data = context.project.find_occurrences(
        args['offset'],
        args['path']).get()

    # TODO: What if it actually fails?
    return {
        'result': 'success',
        'data': data,
    }


@app.post('/findit/implementations')
def findit_implementations_view(context):
    args = bottle.request.json
    data = context.project.find_implementations(
        args['offset'],
        args['path']).get()

    # TODO: What if it actually fails?
    return {
        'result': 'success',
        'data': data,
    }


@app.post('/findit/definition')
def findit_definitions_view(context):
    args = bottle.request.json

    with open(args['path'], 'r') as f:
        code = f.read()

    data = context.project.find_definition(
        code,
        args['offset'],
        args['path']).get()

    # TODO: What if it actually fails?
    return {
        'result': 'success',
        'data': data,
    }


def _importutil_core(context, method):
    # TODO: This patterns of async-tasks is repeated several times.
    # Refactor it.

    args = bottle.request.json
    return standard_async_task(
        context,
        method,
        args['path'])


@app.post("/imports/organize")
def organize_imports_view(context):
    return _importutil_core(
        context,
        context.project.organize_imports)


@app.post("/imports/expand_star")
def expand_star_imports_view(context):
    return _importutil_core(
        context,
        context.project.expand_star_imports)


@app.post("/imports/froms_to_imports")
def from_to_imports_view(context):
    return _importutil_core(
        context,
        context.project.froms_to_imports)


@app.post("/imports/relatives_to_absolutes")
def relatives_to_absolutes_view(context):
    return _importutil_core(
        context,
        context.project.relatives_to_absolutes)


@app.post("/imports/handle_long_imports")
def handle_long_imports_view(context):
    return _importutil_core(
        context,
        context.project.handle_long_imports)


def standard_async_task(context, method, *args):
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
        task_id = next(context.task_ids)
        state = context.state
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
