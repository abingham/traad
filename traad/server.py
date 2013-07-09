from concurrent import futures
import itertools
import logging

from bottle import post, request, run

from .rope.interface import RopeInterface

log = logging.getLogger('traad.server')

# TODO: Is there a way to attach this to every request rather than
# using a global?
project = None

task_ids = itertools.count()
executor = futures.ThreadPoolExecutor(max_workers=1)
tasks = {}

def run_server(port, project_path):
    host = 'localhost'

    log.info(
        'Running traad server for project "{}" at {}:{}'.format(
            project_path,
            host,
            port))

    global project
    project = RopeInterface(project_path)
    run(host=host, port=port)

@post('/refactor/rename')
def rename():
    args = request.json

    log.info('rename: {}'.format(args))

    task_id = next(task_ids)
    tasks[task_id] = executor.submit(
        project.rename,
        new_name=args['name'],
        path=args['path'],
        offset=args.get('offset'))
    return {'task_id': task_id}

def main():
    import argparse

    parser = argparse.ArgumentParser(
        description='Run a traad server.')

    parser.add_argument(
        '-p, --port', metavar='N', type=int,
        dest='port', default=0,
        help='the port on which the server will listen. (0 selects an unused port.)')

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
        log.info('Keyboard interrupt')

if __name__ == '__main__':
    main()
