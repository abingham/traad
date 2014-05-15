import itertools
import logging
import sys

from . import bottle
from .rope.project import Project
from .state import State, TaskState

log = logging.getLogger('traad.server')

# TODO: Is there a way to attach this to every request rather than
# using a global?
project = None

# actor for managing state
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
    state = State.start().proxy()
    project = Project.start(project_path).proxy()

    try:
        bottle.run(host=host, port=port)
    finally:
        project.stop()
        state.stop()

@bottle.get('/root')
def project_root_view():
    return {
        'result': 'success',
        'root': project.get_root().get()
    }

@bottle.get('/all_resources')
def all_resources():
    return {
        'result': 'success',
        'resources': project.get_all_resources().get()
    }

@bottle.get('/task/<task_id>')
def task_status_view(task_id):
    try:
        return state.get_task_state(int(task_id)).get()
    except KeyError:
        bottle.abort(404, "No task with that ID")


@bottle.get('/tasks')
def full_task_status():
    status = state.get_full_state().get()
    log.info('full status: {}'.format(status))
    return status


@bottle.post('/history/undo')
def undo_view():
    args = bottle.request.json
    project.undo(args['index']).get()

    # TODO: What if it actually fails?
    return {'result': 'success'}


@bottle.post('/history/redo')
def redo_view():
    args = bottle.request.json
    project.redo(args['index']).get()

    # TODO: What if it actually fails?
    return {'result': 'success'}


@bottle.get('/history/view_undo')
def undo_history_view():
    return {
        'result': 'success',
        'history': project.undo_history().get()
    }


@bottle.get('/history/view_redo')
def redo_history_view():
    return {
        'result': 'success',
        'history': project.redo_history().get()
    }


@bottle.get('/history/undo_info/<idx>')
def undo_info_view(idx):
    return {
        'result': 'success',
        'info': project.undo_info(int(idx)).get()
    }


@bottle.get('/history/redo_info/<idx>')
def redo_info_view(idx):
    return {
        'result': 'success',
        'info': project.redo_info(int(idx)).get()
    }


@bottle.get('/test/long_running')
def long_running_test():
    import traad.test.tasks as tasks
    args = bottle.request.json

    return standard_async_task(tasks.long_running,
                               args['message'])

@bottle.post('/refactor/rename')
def rename_view():
    args = bottle.request.json
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


@bottle.post('/refactor/extract_method')
def extract_method_view():
    return extract_core(project.extract_method, bottle.request)


@bottle.post('/refactor/extract_variable')
def extract_variable_view():
    return extract_core(project.extract_variable, bottle.request)


@bottle.post('/refactor/normalize_arguments')
def normalize_arguments_view():
    args = bottle.request.json
    return standard_async_task(project.normalize_arguments,
                               args['path'],
                               args['offset'])


@bottle.post('/refactor/remove_argument')
def remove_argument_view():
    args = bottle.request.json
    return standard_async_task(project.remove_argument,
                               args['arg_index'],
                               args['path'],
                               args['offset'])


@bottle.post('/code_assist/completions')
def code_assist_completion_view():
    args = bottle.request.json

    log.info('get completion: {}'.format(args))

    results = project.code_assist(args['code'],
                                  args['offset'],
                                  args['path']).get()

    # TODO: What if it fails?
    return {
        'result': 'success',
        'completions': results,
    }


@bottle.post('/code_assist/doc')
def code_assist_doc_view():
    args = bottle.request.json

    log.info('get doc: {}'.format(args))

    doc = project.get_doc(
        code=args['code'],
        offset=args['offset'],
        path=args['path']).get()

    return {
        'result': 'failure' if doc is None else 'success',
        'doc': doc
    }


@bottle.post('/code_assist/calltip')
def code_assist_calltip_view():
    args = bottle.request.json

    log.info('get calltip: {}'.format(args))

    calltip = project.get_calltip(
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
#         'results': project.get_definition_location(
#             code=args['code'],
#             offset=args['offset'],
#             path=args['path'])
#     }


@bottle.post('/findit/occurrences')
def findit_occurences_view():
    args = bottle.request.json
    data = project.find_occurrences(
        args['offset'],
        args['path']).get()

    # TODO: What if it actually fails?
    return {
        'result': 'success',
        'data': data,
    }


@bottle.post('/findit/implementations')
def findit_implementations_view():
    args = bottle.request.json
    data = project.find_implementations(
        args['offset'],
        args['path']).get()

    # TODO: What if it actually fails?
    return {
        'result': 'success',
        'data': data,
    }


@bottle.post('/findit/definition')
def findit_definitions_view():
    args = bottle.request.json
    data = project.find_definition(
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
    print(args)
    return standard_async_task(
        method,
        args['path'])


@bottle.post("/imports/organize")
def organize_imports_view():
    return _importutil_core(bottle.request, project.organize_imports)


@bottle.post("/imports/expand_star")
def expand_star_imports_view():
    return _importutil_core(bottle.request, project.expand_star_imports)


@bottle.post("/imports/froms_to_imports")
def from_to_imports_view():
    return _importutil_core(bottle.request, project.froms_to_imports)


@bottle.post("/imports/relatives_to_absolutes")
def relatives_to_absolutes_view():
    return _importutil_core(bottle.request, project.relatives_to_absolutes)


@bottle.post("/imports/handle_long_imports")
def handle_long_imports_view():
    return _importutil_core(bottle.request, project.handle_long_imports)


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

if __name__ == '__main__':
    main()
