from concurrent import futures
import itertools
import logging

from bottle import post, request, run

from .rope.interface import RopeInterface

log = logging.getLogger('traad.server')

# def run_server(port, project):
#     server = SimpleXMLRPCServer(
#         ('127.0.0.1', port),
#         logRequests=True,
#         allow_none=True)

#     print(server.server_address[1])

#     log.info(
#         'Running traad server for project "{}" at {}:{}'.format(
#             project,
#             server.server_address[0],
#             server.server_address[1]))

#     server.register_instance(
#         RopeInterface(project))

#     server.serve_forever()

project = None
task_ids = itertools.count()
executor = futures.ThreadPoolExecutor(max_workers=1)
tasks = {}

@post('/refactor/rename')
def rename():
    print(request.json)
    args = request.json
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
        global project
        project = RopeInterface(args.project)
        run(host='localhost', port=args.port)
    except KeyboardInterrupt:
        log.info('Keyboard interrupt')

if __name__ == '__main__':
    main()
