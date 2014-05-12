import itertools
import logging
import sys

try:
    from queue import Queue
except ImportError:
    from Queue import Queue

from .bottle import abort, get, request, run

from .rope.project import Project
from .state import State
from .task import AsyncTask
from .task_processor import TaskProcessor


log = logging.getLogger('traad.server')

# TODO: Is there a way to attach this to every request rather than
# using a global?
project_ref = None
project = None

# actor for managing state
state_ref = None
state = None

task_ids = itertools.count()

def run_server(port, project_path):
    host = 'localhost'

    log.info('Python version: {}'.format(sys.version))

    log.info(
        'Running traad server for project "{}" at {}:{}'.format(
            project_path,
            host,
            port))

    global project, project_ref, state, state_ref
    state_ref = State.start()
    state = state_ref.proxy()
    project_ref = Project.start(project_path)
    project = project_ref.proxy()

    try:
        run(host=host, port=port)
    finally:
        project.stop()
        state.stop()


@get('/root')
def project_root_view():
    return {
        'result': 'success',
        'root': project.get_root().get()
    }

@get('/all_resources')
def all_resources():
    return {
        'result': 'success',
        'resources': project.get_all_resources().get()
    }

@get('/task/<task_id>')
def task_status_view(task_id):
    try:
        return state.get_task_state(int(task_id)).get()
    except KeyError:
        abort(404, "No task with that ID")


@get('/tasks')
def full_task_status():
    status = state.get_full_state().get()
    log.info('full status: {}'.format(status))
    return status


@get('/history/undo')
def undo_view():
    args = request.json
    project.undo(args['index'])

    # TODO: What if it actually fails?
    return {'result': 'success'}


@get('/history/redo')
def redo_view():
    args = request.json
    project.redo(args['index'])

    # TODO: What if it actually fails?
    return {'result': 'success'}


@get('/history/view_undo')
def undo_history_view():
    return {
        'result': 'success',
        'history': project.undo_history().get()
    }


@get('/history/view_redo')
def redo_history_view():
    return {
        'result': 'success',
        'history': project.redo_history().get()
    }


@get('/history/undo_info/<idx>')
def undo_info_view(idx):
    return {
        'result': 'success',
        'info': project.undo_info(int(idx)).get()
    }


@get('/history/redo_info/<idx>')
def redo_info_view(idx):
    return {
        'result': 'success',
        'info': project.redo_info(int(idx)).get()
    }


# @get('/test/long_running')
# def long_running_test():
#     import traad.test.tasks as tasks
#     args = request.json

#     return standard_async_task(tasks.long_running,
#                                args['message'])

@get('/refactor/rename')
def rename_view():
    from .rope.rename import rename
    args = request.json
    return standard_async_task(project.rename,
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


@get('/refactor/extract_method')
def extract_method_view():
    from .rope.extract import extract_method
    return extract_core(extract_method, request)


@get('/refactor/extract_variable')
def extract_variable_view():
    from .rope.extract import extract_variable
    return extract_core(extract_variable, request)


@get('/refactor/normalize_arguments')
def normalize_arguments_view():
    from .rope.change_signature import normalize_arguments
    args = request.json
    return standard_async_task(normalize_arguments,
                               args['path'],
                               args['offset'])


@get('/refactor/remove_argument')
def remove_argument_view():
    from .rope.change_signature import remove_argument
    args = request.json
    return standard_async_task(remove_argument,
                               args['arg_index'],
                               args['path'],
                               args['offset'])


@get('/code_assist/completions')
def code_assist_completion_view():
    from traad.rope.codeassist import code_assist

    args = request.json

    log.info('get completion: {}'.format(args))

    results = code_assist(project,
                          args['code'],
                          args['offset'],
                          args['path'])

    # TODO: What if it fails?
    return {
        'result': 'success',
        'completions': results,
    }


@get('/code_assist/doc')
def code_assist_doc_view():
    from traad.rope.codeassist import get_doc

    args = request.json

    log.info('get doc: {}'.format(args))

    doc = get_doc(
        project,
        code=args['code'],
        offset=args['offset'],
        path=args['path'])

    return {
        'result': 'failure' if doc is None else 'success',
        'doc': doc
    }


@get('/code_assist/calltip')
def code_assist_calltip_view():
    from traad.rope.codeassist import get_calltip

    args = request.json

    log.info('get calltip: {}'.format(args))

    calltip = get_calltip(
        project,
        code=args['code'],
        offset=args['offset'],
        path=args['path'])

    return {
        'result': 'failure' if calltip is None else 'success',
        'calltip': calltip
    }


# @get('/code_assist/definition')
# def code_assist_definition_view():
#     args = request.json

#     log.info('get definition: {}'.format(args))

#     return {
#         'results': project.get_definition_location(
#             code=args['code'],
#             offset=args['offset'],
#             path=args['path'])
#     }


@get('/findit/occurrences')
def findit_occurences_view():
    from traad.rope.findit import find_occurrences

    args = request.json
    data = find_occurrences(project, args['offset'], args['path'])

    # TODO: What if it actually fails?
    return {
        'result': 'success',
        'data': data,
    }


@get('/findit/implementations')
def findit_implementations_view():
    from traad.rope.findit import find_implementations

    args = request.json
    data = find_implementations(project, args['offset'], args['path'])

    # TODO: What if it actually fails?
    return {
        'result': 'success',
        'data': data,
    }


@get('/findit/definition')
def findit_definitions_view():
    from traad.rope.findit import find_definition

    args = request.json
    data = find_definition(project,
                           args['code'],
                           args['offset'],
                           args['path'])

    # TODO: What if it actually fails?
    return {
        'result': 'success',
        'data': data,
    }


def _importutil_core(request, method):
    # TODO: This patterns of async-tasks is repeated several times.
    # Refactor it.

    args = request.json
    print(args)
    return standard_async_task(
        method,
        args['path'])


@get("/imports/organize")
def organize_imports_view():
    from traad.rope.importutil import organize_imports
    return _importutil_core(request, organize_imports)


@get("/imports/expand_star")
def expand_star_imports_view():
    from traad.rope.importutil import expand_star_imports
    return _importutil_core(request, expand_star_imports)


@get("/imports/froms_to_imports")
def from_to_imports_view():
    from traad.rope.importutil import froms_to_imports
    return _importutil_core(request, froms_to_imports)


@get("/imports/relatives_to_absolutes")
def relatives_to_absolutes_view():
    from traad.rope.importutil import relatives_to_absolutes
    return _importutil_core(request, relatives_to_absolutes)


@get("/imports/handle_long_imports")
def handle_long_imports_view():
    from traad.rope.importutil import handle_long_imports
    return _importutil_core(request, handle_long_imports)


def standard_async_task(method, *args):
    """Launch a typical async task.

    This runs ``method`` in an ``AsyncTask``, returning a dict with
    the task-id. Information about the progress is logged. Errors are
    detected, logged, and a proper result is returned.

    Args:
      method: The callable to execute in the ``AsyncTask``.
      args: The arguments to pass to ``method``.

    """
    log.info('{}: {}'.format(method, args))

    try:
        task_id = next(task_ids)
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


def main():
    import argparse

    parser = argparse.ArgumentParser(
        description='Run a traad server.')

    parser.add_argument(
        '-p, --port', metavar='N', type=int,
        dest='port', default=0,
        help='the port on which the server will listen. '
             '(0 selects an unused port.)')

    parser.add_argument(
        '-V, --verbosity', metavar='N', type=int,
        dest='verbosity', default=0,
        help='Verbosity level (0=normal, 1=info, 2=debug).')

    parser.add_argument(
        'project', metavar='P', type=str,
        help='the directory containing the project to serve')

    args = parser.parse_args()

    # Configure logging
    level = {
        0: logging.WARNING,
        1: logging.INFO,
        2: logging.DEBUG
    }[args.verbosity]

    logging.basicConfig(
        level=level)

    try:
        run_server(args.port, args.project)
    except KeyboardInterrupt:
        # TODO: Executor shutdown?
        log.info('Keyboard interrupt')
    finally:
        state.stop()

if __name__ == '__main__':
    main()
