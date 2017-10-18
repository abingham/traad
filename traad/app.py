from contextlib import contextmanager
from functools import wraps
import logging
import sys

import rope.contrib.codeassist
from rope.refactor.change_signature import (ArgumentAdder,
                                            ArgumentNormalizer,
                                            ArgumentRemover,
                                            ChangeSignature)
import rope.refactor.extract
import rope.refactor.inline
import rope.refactor.rename

from . import bottle
from .plugin import TraadPlugin
from .state import TaskState
from .rope.workspace import changes_to_data, data_to_changes


log = logging.getLogger('traad.app')

PROTOCOL_VERSION = 2

app = bottle.Bottle()


@contextmanager
def using_workspace(project_path, app=app):
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


@app.post('/history/undo')
def undo_view(context):
    args = bottle.request.json
    changes = context.workspace.undo(args['index'])

    return {'result': 'success',
            'changes': [changes_to_data(c) for c in changes]}


@app.post('/history/redo')
def redo_view(context):
    args = bottle.request.json
    changes = context.workspace.redo(args['index'])

    # TODO: What if it actually fails?
    return {'result': 'success',
            'changes': [changes_to_data(c) for c in changes]}


@app.get('/history/view_undo')
def undo_history_view(context):
    return {
        'result': 'success',
        'history': context.workspace.undo_history()
    }


@app.get('/history/view_redo')
def redo_history_view(context):
    return {
        'result': 'success',
        'history': context.workspace.redo_history()
    }


@app.get('/history/undo_info/<idx>')
def undo_info_view(idx, context):
    return {
        'result': 'success',
        'info': context.workspace.undo_info(int(idx))
    }


@app.get('/history/redo_info/<idx>')
def redo_info_view(idx, context):
    return {
        'result': 'success',
        'info': context.workspace.redo_info(int(idx))
    }


@app.post('/test/long_running')
def long_running_test(context):
    import traad.test
    args = bottle.request.json

    return standard_async_task(
        context,
        traad.test.long_running,
        args['message'])


# TODO: COmmon exception handler decorator? Middleware?
@app.post('/refactor/perform')
def perform_view(context):
    args = bottle.request.json

    try:
        changes = args['changes']
        changes = data_to_changes(context.workspace, changes)
        context.workspace.perform(changes)
        return {
            'result': 'success'
        }
    except:
        e = sys.exc_info()[1]
        log.error('perform error: {}'.format(e))
        return {
            'result': 'failure',
            'message': str(e)
        }


def _basic_refactoring(context,
                       refactoring,
                       refactoring_args,
                       change_args):
    try:
        changes = context.workspace.get_changes(
            refactoring,
            refactoring_args,
            change_args)

        return {
            'result': 'success',
            'changes': changes_to_data(changes)
        }
    except:
        log.exception('{} error'.format(refactoring))
        return {
            'result': 'failure',
            'message': str(sys.exc_info()[1])
        }


def standard_refactoring(f):
    @wraps(f)
    def wrapper(context, *args, **kwargs):
        try:
            changes = f(context, *args, **kwargs)

            return {
                'result': 'success',
                'changes': changes_to_data(changes)
            }
        except:
            log.exception('{} error'.format('rename'))
            return {
                'result': 'failure',
                'message': str(sys.exc_info()[1])
            }

    return wrapper


#  TODO: Should this be a GET? We're not making any changes.
@app.post('/refactor/rename')
@standard_refactoring
def rename_view(context):
    args = bottle.request.json
    return context.workspace.rename(
        args['path'],
        args.get('offset'),
        args['name'])


@app.post('/refactor/extract_method')
@standard_refactoring
def extract_method_view(context):
    args = bottle.request.json
    return context.workspace.extract_method(
        args['path'],
        args['start-offset'],
        args['end-offset'],
        args['name'])


@app.post('/refactor/extract_variable')
@standard_refactoring
def extract_variable_view(context):
    args = bottle.request.json
    return context.workspace.extract_variable(
        args['path'],
        args['start-offset'],
        args['end-offset'],
        args['name'])


@app.post('/refactor/inline')
@standard_refactoring
def inline_view(context):
    args = bottle.request.json
    return context.workspace.inline(
        args['path'],
        args['offset'])


@app.post('/refactor/normalize_arguments')
@standard_refactoring
def normalize_arguments_view(context):
    args = bottle.request.json
    return context.workspace.normalize_arguments(
        args['path'],
        args['offset'])


def asdffdsa(bar): pass


@app.post('/refactor/remove_argument')
@standard_refactoring
def remove_argument_view(context):
    args = bottle.request.json
    return context.workspace.remove_argument(
        args['path'],
        args['offset'],
        args['arg_index'])


@app.post('/refactor/add_argument')
def add_argument_view(context):
    args = bottle.request.json
    changers = [ArgumentAdder(args['index'],
                              args['name'],
                              args['default'],
                              args['value'])]
    return _basic_refactoring(
        context,
        ChangeSignature,
        path=args['path'],
        refactoring_args=(args['offset'],),
        change_args=(changers,))


@app.post('/code_assist/completions')
def code_assist_completion_view(context):
    args = bottle.request.json

    log.info('get completion: {}'.format(args))

    with open(args['path'], 'r') as f:
        code = f.read()

    results = context.workspace.code_assist(
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

    doc = context.workspace.get_doc(
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

    calltip = context.workspace.get_calltip(
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
#         'results': context.workspace.get_definition_location(
#             code=args['code'],
#             offset=args['offset'],
#             path=args['path'])
#     }


@app.post('/findit/occurrences')
def findit_occurences_view(context):
    args = bottle.request.json
    data = context.workspace.find_occurrences(
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
    data = context.workspace.find_implementations(
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

    data = context.workspace.find_definition(
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
        context.workspace.organize_imports)


@app.post("/imports/expand_star")
def expand_star_imports_view(context):
    return _importutil_core(
        context,
        context.workspace.expand_star_imports)


@app.post("/imports/froms_to_imports")
def from_to_imports_view(context):
    return _importutil_core(
        context,
        context.workspace.froms_to_imports)


@app.post("/imports/relatives_to_absolutes")
def relatives_to_absolutes_view(context):
    return _importutil_core(
        context,
        context.workspace.relatives_to_absolutes)


@app.post("/imports/handle_long_imports")
def handle_long_imports_view(context):
    return _importutil_core(
        context,
        context.workspace.handle_long_imports)


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
