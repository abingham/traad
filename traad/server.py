import logging

from .rope.interface import RopeInterface
from .xmlrpc import SimpleXMLRPCServer

log = logging.getLogger('traad.server')

def run_server(port, project):
    server = SimpleXMLRPCServer(
        ('127.0.0.1', port),
        logRequests=True,
        allow_none=True)

    print(server.server_address[1])

    log.info(
        'Running traad server for project "{}" at {}:{}'.format(
            project,
            server.server_address[0],
            server.server_address[1]))

    server.register_instance(
        RopeInterface(project))

    server.serve_forever()

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
