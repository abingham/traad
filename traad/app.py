import logging
import sys
from contextlib import contextmanager
from functools import wraps

from . import bottle
from .plugin import RopeWorkspacePlugin
from .rope.thing_at import thing_at
from .rope.workspace import changes_to_data, data_to_changes


log = logging.getLogger('traad.app')

PROTOCOL_VERSION = 3

app = bottle.Bottle()


@app.get('/protocol_version')
def protocol_version_view():
    return {'protocol-version': PROTOCOL_VERSION}


@app.get('/root')
def root_view(context):
    return {'root': context.workspace.root_project.root.real_path}


@app.post('/thing_at')
def thing_at_view(context):
    args = bottle.request.json
    thing = thing_at(
        context.workspace.root_project,
        context.workspace.get_resource(args['path']),
        offset=args.get('offset'))
    return {'thing': thing.value if thing is not None else ''}


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


# TODO: Common exception handler decorator? Middleware?
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
            log.exception('error')
            return {
                'result': 'failure',
                'message': str(sys.exc_info()[1])
            }

    return wrapper


@app.post('/refactor/move_global')
@standard_refactoring
def move_global_view(context):
    args = bottle.request.json
    return context.workspace.move_global(
        args['path'],
        args['offset'],
        args['dest'])


@app.post('/refactor/move_module')
@standard_refactoring
def move_module_view(context):
    args = bottle.request.json
    return context.workspace.move_module(
        args['path'],
        args['dest'])


@app.post('/auto_import/get_imports')
def get_imports_view(context):
    args = bottle.request.json
    return context.workspace.get_imports(
        args['path'],
        args['offset'])

#  TODO: Should this be a GET? We're not making any changes.
@app.post('/refactor/rename')
@standard_refactoring
def rename_view(context):
    args = bottle.request.json

    log.info("`rename_view` called: {}".format(args))

    # TODO 1: Modify "emacs-traad" to use these parameters.
    docs = bool(args.get('docs', False))
    unsure = args.get('unsure', None)
    in_hierarchy = bool(args.get('in_hierarchy', False))

    return context.workspace.rename(
        args['path'],
        args.get('offset'),
        args['name'],
        docs=docs,
        in_hierarchy=in_hierarchy,
        unsure=unsure)

@app.post('/refactor/move')
@standard_refactoring
def move_view(context):
    args = bottle.request.json
    return context.workspace.move(
        args['path'],
        args.get('offset'),
        args['dest'])

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


@app.post('/refactor/introduce_parameter')
@standard_refactoring
def introduce_parameter_view(context):
    args = bottle.request.json
    return context.workspace.introduce_parameter(
        args['path'],
        args['offset'],
        args['parameter'])


@app.post('/refactor/encapsulate_field')
@standard_refactoring
def encapsulate_field_view(context):
    args = bottle.request.json
    return context.workspace.encapsulate_field(
        args['path'],
        args['offset'])


@app.post('/refactor/local_to_field')
@standard_refactoring
def local_to_field_view(context):
    args = bottle.request.json
    return context.workspace.local_to_field(
        args['path'],
        args['offset'])


@app.post('/refactor/use_function')
@standard_refactoring
def use_function_view(context):
    args = bottle.request.json
    return context.workspace.use_function(
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


@app.post('/code_assist/definition')
def code_assist_definition_view(context):
    args = bottle.request.json

    log.info('get definition: {}'.format(args))

    if 'code' in args:
        code = args['code']
    else:
        with open(args['path'], 'r') as f:
            code = f.read()

    definition, line = context.workspace.get_definition_location(
        code=code,
        offset=args['offset'],
        path=args['path'])

    if definition is None:
        return {
            'result': "failure",
            'realpath': None,
            'path': None,
            'name': None,
            'lineno': None,
        }
    else:
        return {
            'result': "success",
            'realpath': definition.real_path,
            'path': definition.path,
            'name': definition.name,
            'lineno': line,
        }


@app.get('/findit/occurrences')
def findit_occurrences_view(context):
    args = bottle.request.json

    log.info('findit occurrences: {}'.format(args))

    locations = context.workspace.find_occurrences(
        args['offset'],
        args['path'])

    if locations is None:
        return {
            'result': 'failure',
            'locations': None,
        }
    else:
        return {
            'result': 'success',
            'locations': locations,
        }


@app.get('/findit/implementations')
def findit_implementations_view(context):
    args = bottle.request.json

    log.info('findit implementations: {}'.format(args))

    locations = context.workspace.find_implementations(
        args['offset'],
        args['path'])

    if locations is None:
        return {
            'result': 'failure',
            'locations': None,
        }
    else:
        return {
            'result': 'success',
            'locations': locations,
        }


@app.get('/findit/definition')
def findit_definition_view(context):
    args = bottle.request.json

    log.info('findit definition: {}'.format(args))

    with open(args['path'], 'r') as f:
        code = f.read()

    location = context.workspace.find_definition(
        code,
        args['offset'],
        args['path'])

    if location is None:
        return {
            'result': 'failure',
            'location': None,
        }
    else:
        return {
            'result': 'success',
            'location': location,
        }


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
