import itertools
import logging
import queue

from bottle import abort, get, hook, post, request, run

from .rope.project import Project
from .rope.rename import rename
from .state import State
from .task import AsyncTask
from .task_processor import TaskProcessor


log = logging.getLogger('traad.server')

# TODO: Is there a way to attach this to every request rather than
# using a global?
project = None

state = State()

task_ids = itertools.count()
task_queue = queue.Queue()


def run_server(port, project_path):
    host = 'localhost'

    log.info(
        'Running traad server for project "{}" at {}:{}'.format(
            project_path,
            host,
            port))

    global project
    project = Project(project_path)
    proc = TaskProcessor(task_queue, project, state)
    proc.start()

    try:
        run(host=host, port=port)
    finally:
        task_queue.put(None)
        task_queue.join()


# def task_status(task_id):
#     task = tasks[int(task_id)]

#     if task.cancelled():
#         return {'status': 'CANCELLED'}
#     elif task.running():
#         return {'status': 'RUNNING'}
#     elif task.done():
#         return {'status': 'COMPLETE'}
#     else:
#         return {'status': 'PENDING'}


# @hook('before_request')
# def before_req():
#     pass


@get('/task/<task_id>')
def task_status_view(task_id):
    try:
        return state.get_task_state(int(task_id)).get()
    except KeyError:
        abort(404, "No task with that ID")


@get('/tasks')
def full_task_status():
    status = state.get_full_state()
    log.info('full status: {}'.format(status))
    return status


@post('/test/long_running')
def long_running_test():
    import traad.test.tasks as tasks
    args = request.json

    log.info('long running test: {}'.format(args))

    task_id = next(task_ids)
    state.create(task_id)

    task_queue.put(
        AsyncTask(
            project,
            state,
            task_id,
            tasks.long_running,
            args['message']))

    return {'task_id': task_id}


@post('/refactor/rename')
def rename_view():
    args = request.json

    log.info('rename: {}'.format(args))

    try:
        task_id = next(task_ids)
        state.create(task_id)

        task_queue.put(
            AsyncTask(
                project,
                state,
                task_id,
                rename,
                args['name'],
                args['path'],
                args.get('offset')))

        log.info('rename success')

        return {
            'result': 'ok',
            'task_id': task_id
        }
    except Exception as e:
        log.error('rename error: {}'.format(e))
        return {
            'result': 'fail',
            'message': str(e)
        }


# @post('/refactor/normalize_arguments')
# def normalize_arguments_view():
#     args = request.json

#     log.info('normalize arguments: {}'.format(args))

#     task_id = next(task_ids)
#     tasks[task_id] = executor.submit(
#         project.normalize_arguments,
#         path=args['path'],
#         offset=args['offset'])
#     return {'task_id': task_id}


# @post('/refactor/remove_argument')
# def remove_argument_view():
#     args = request.json

#     log.info('remove argument: {}'.format(args))

#     task_id = next(task_ids)
#     tasks[task_id] = executor.submit(
#         project.remove_argument,
#         arg_index=args['arg_index'],
#         path=args['path'],
#         offset=args['offset'])
#     return {'task_id': task_id}


# @get('/code_assist/completion')
# def code_assist_completion_view():
#     args = request.json

#     log.info('code assist: {}'.format(args))

#     return {
#         'results': project.code_assist(
#             code=args['code'],
#             offset=args['offset'],
#             path=args['path'])
#     }


# @get('/code_assist/doc')
# def code_assist_doc_view():
#     args = request.json

#     log.info('get doc: {}'.format(args))

#     return {
#         'results': project.get_doc(
#             code=args['code'],
#             offset=args['offset'],
#             path=args['path'])
#     }


# @get('/code_assist/calltip')
# def code_assist_calltip_view():
#     args = request.json

#     log.info('get calltip: {}'.format(args))

#     return {
#         'results': project.get_calltip(
#             code=args['code'],
#             offset=args['offset'],
#             path=args['path'])
#     }


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
