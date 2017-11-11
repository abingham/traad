import logging
import sys
from contextlib import contextmanager
from functools import wraps

from . import bottle
from .plugin import TraadPlugin
from .rope.workspace import changes_to_data, data_to_changes


log = logging.getLogger('traad.app')

PROTOCOL_VERSION = 3

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


@app.get('/root')
def root_view(context):
    return {'root': context.workspace.root_project.root.real_path}


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
        log.exception('perform error: {}'.format(e))
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


@app.post('/refactor/remove_argument')
@standard_refactoring
def remove_argument_view(context):
    args = bottle.request.json
    return context.workspace.remove_argument(
        args['path'],
        args['offset'],
        args['arg_index'])


@app.post('/refactor/add_argument')
@standard_refactoring
def add_argument_view(context):
    args = bottle.request.json
    return context.workspace.add_argument(
        args['path'],
        args['offset'],
        args['arg_index'],
        args['name'],
        args['default'],
        args['value'])


@app.post('/code_assist/completions')
def code_assist_completion_view(context):
    args = bottle.request.json

    log.info('get completion: {}'.format(args))

    with open(args['path'], 'r') as f:
        code = f.read()

    results = context.workspace.code_assist(
        code,
        args['offset'],
        args['path'])

    # TODO: What if it fails?
    print(results)
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
        path=args['path'])

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
        path=args['path'])

    return {
        'result': 'failure' if calltip is None else 'success',
        'calltip': calltip
    }


@app.get('/code_assist/definition')
def code_assist_definition_view():
    args = request.json

    log.info('get definition: {}'.format(args))

    return {
        'results': context.workspace.get_definition_location(
            code=args['code'],
            offset=args['offset'],
            path=args['path'])
    }


# @app.post('/findit/occurrences')
# def findit_occurences_view(context):
#     args = bottle.request.json
#     data = context.workspace.find_occurrences(
#         args['offset'],
#         args['path'])

#     # TODO: What if it actually fails?
#     return {
#         'result': 'success',
#         'data': data,
#     }


# @app.post('/findit/implementations')
# def findit_implementations_view(context):
#     args = bottle.request.json
#     data = context.workspace.find_implementations(
#         args['offset'],
#         args['path'])

#     # TODO: What if it actually fails?
#     return {
#         'result': 'success',
#         'data': data,
#     }


# @app.post('/findit/definition')
# def findit_definitions_view(context):
#     args = bottle.request.json

#     with open(args['path'], 'r') as f:
#         code = f.read()

#     data = context.workspace.find_definition(
#         code,
#         args['offset'],
#         args['path'])

#     # TODO: What if it actually fails?
#     return {
#         'result': 'success',
#         'data': data,
#     }


@app.post("/imports/organize")
@standard_refactoring
def organize_imports_view(context):
    args = bottle.request.json
    return context.workspace.organize_imports(
        args['path'])


@app.post("/imports/expand_stars")
@standard_refactoring
def expand_star_imports_view(context):
    args = bottle.request.json
    return context.workspace.expand_star_imports(
        args['path'])


@app.post("/imports/froms_to_imports")
@standard_refactoring
def from_to_imports_view(context):
    args = bottle.request.json
    return context.workspace.froms_to_imports(
        args['path'])


@app.post("/imports/relatives_to_absolutes")
@standard_refactoring
def relatives_to_absolutes_view(context):
    args = bottle.request.json
    return context.workspace.relatives_to_absolutes(
        args['path'])


@app.post("/imports/handle_long_imports")
@standard_refactoring
def handle_long_imports_view(context):
    args = bottle.request.json
    return context.workspace.handle_long_imports(
        args['path'])
