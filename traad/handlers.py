from functools import wraps
import logging
import sys

from aiohttp import web

from .state import TaskState


log = logging.getLogger('traad.handlers')

PROTOCOL_VERSION = 2


def json(f):
    @wraps(f)
    async def wrapper(*args, **kwargs):
        result = await f(*args, **kwargs)
        return web.json_response(result)
    return wrapper


@json
async def protocol_version(request):
    return {'protocol-version': PROTOCOL_VERSION}


@json
async def root(request):
    try:
        return {
            'result': 'success',
            'root': request.app['project'].get_root()
        }
    except Exception:
        raise ValueError('huh?')


@json
async def all_resources(request):
    return {
        'result': 'success',
        'resources': request.app['project'].get_all_resources()
    }


@json
async def task_status(request):
    task_id = request.match_info['task_id']
    try:
        return request.app['state'].get_task_state(int(task_id))
    except KeyError:
        raise KeyError("No task with ID={}".format(task_id))


@json
async def tasks(request):
    status = request.app['state'].get_full_state()
    log.info('full status: {}'.format(status))
    return status


@json
async def undo(request):
    args = await request.json()
    request.app['project'].undo(args['index'])

    # TODO: What if it actually fails?
    return {'result': 'success'}


@json
async def redo(request):
    args = await request.json()
    request.app['project'].redo(args['index']).get()

    # TODO: What if it actually fails?
    return {'result': 'success'}


@json
async def undo_history(request):
    return {
        'result': 'success',
        'history': request.app['project'].undo_history().get()
    }


@json
async def redo_history(request):
    return {
        'result': 'success',
        'history': request.app['project'].redo_history()
    }


@json
async def undo_info(request):
    idx = request.match_info['idx']
    return {
        'result': 'success',
        'info': request.app['project'].undo_info(int(idx)).get()
    }


@json
async def redo_info(request):
    idx = request.match_info['idx']
    return {
        'result': 'success',
        'info': request.app['project'].redo_info(int(idx)).get()
    }


async def long_running_test(request):
    import traad.test
    args = await request.json()

    return standard_task(
        request,
        traad.test.long_running,
        args['message'])


async def rename(request):
    args = await request.json()
    return standard_task(
        request,
        request.app['project'].rename,
        args['name'],
        args['path'],
        args.get('offset'))


async def extract_core(method, request):
    """Common implementation for extract-method and extract-variable views.

    Args:
      method: The refactoring method to actually call.
      request: The bottle request for the refactoring.
    """
    args = await request.json()
    return standard_task(
        request,
        method,
        args['name'],
        args['path'],
        args['start-offset'],
        args['end-offset'])


async def extract_method(request):
    return await extract_core(
        request.app['project'].extract_method,
        request)


async def extract_variable(request):
    return await extract_core(
        request.app['project'].extract_variable,
        request)


async def normalize_arguments(request):
    args = await request.json()
    return standard_task(
        request,
        request.app['project'].normalize_arguments,
        args['path'],
        args['offset'])


async def remove_argument(request):
    args = await request.json()
    return standard_task(
        request,
        request.app['project'].remove_argument,
        args['arg_index'],
        args['path'],
        args['offset'])


async def add_argument(request):
    args = await request.json()
    return standard_task(
        request,
        request.app['project'].add_argument,
        args['path'],
        args['offset'],
        args['index'],
        args['name'],
        args['default'],
        args['value'])


@json
async def code_assist_completion(request):
    args = await request.json()

    log.info('get completion: {}'.format(args))

    # TODO: Do this async?
    with open(args['path'], 'r') as f:
        code = f.read()

    results = request.app['project'].code_assist(
        code,
        args['offset'],
        args['path']).get()

    # TODO: What if it fails?
    return {
        'result': 'success',
        'completions': results,
    }


@json
async def code_assist_doc(request):
    args = await request.json()

    log.info('get doc: {}'.format(args))

    with open(args['path'], 'r') as f:
        code = f.read()

    doc = request.app['project'].get_doc(
        code=code,
        offset=args['offset'],
        path=args['path']).get()

    return {
        'result': 'failure' if doc is None else 'success',
        'doc': doc
    }


@json
async def code_assist_calltip(request):
    args = await request.json()

    log.info('get calltip: {}'.format(args))

    with open(args['path'], 'r') as f:
        code = f.read()

    calltip = request.app['project'].get_calltip(
        code=code,
        offset=args['offset'],
        path=args['path']).get()

    return {
        'result': 'failure' if calltip is None else 'success',
        'calltip': calltip
    }


# @get('/code_assist/definition')
# def code_assist_definition():
#     args = request.json

#     log.info('get definition: {}'.format(args))

#     return {
#         'results': request.app['project'].get_definition_location(
#             code=args['code'],
#             offset=args['offset'],
#             path=args['path'])
#     }


@json
async def findit_occurences(request):
    args = await request.json()
    data = request.app['project'].find_occurrences(
        args['offset'],
        args['path']).get()

    # TODO: What if it actually fails?
    return {
        'result': 'success',
        'data': data,
    }


@json
async def findit_implementations(request):
    args = await request.json()
    data = request.app['project'].find_implementations(
        args['offset'],
        args['path']).get()

    # TODO: What if it actually fails?
    return {
        'result': 'success',
        'data': data,
    }


@json
async def findit_definitions(request):
    args = await request.json()

    with open(args['path'], 'r') as f:
        code = f.read()

    data = request.app['project'].find_definition(
        code,
        args['offset'],
        args['path']).get()

    # TODO: What if it actually fails?
    return {
        'result': 'success',
        'data': data,
    }


async def _importutil_core(request, method):
    # TODO: This patterns of async-tasks is repeated several times.
    # Refactor it.

    args = await request.json()
    return await standard_task(
        method,
        args['path'])


async def organize_imports(request):
    return await _importutil_core(
        request,
        request.app['project'].organize_imports)


async def expand_star_imports(request):
    return await _importutil_core(
        request,
        request.app['project'].expand_star_imports)


async def from_to_imports(request):
    return _importutil_core(
        request,
        request.app['project'].froms_to_imports)


async def relatives_to_absolutes(request):
    return _importutil_core(
        request,
        request.app['project'].relatives_to_absolutes)


async def handle_long_imports(request):
    return _importutil_core(
        request,
        request.app['project'].handle_long_imports)


@json
async def standard_task(request, method, *args):
    """Launch a typical task.

    This creates a `TaskState` for the new task and runs the task.

    Args:
      method: The asynchronous callable to execute.
      args: The arguments to pass to ``method``.
    """
    log.info('{}: {}'.format(method, args))

    try:
        task_id = next(request.app['task_ids'])
        state = request.app['state']
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
