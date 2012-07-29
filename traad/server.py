import logging

from .rope_interface import RopeInterface
from .xmlrpc import SimpleXMLRPCServer

log = logging.getLogger(__name__)


def run_server(port, project):
    log.info(
        'Running traad server for project "{}" on port {}'.format(
            project, port))

    server = SimpleXMLRPCServer(
        ('127.0.0.1', port),
        logRequests=True,
        allow_none=True)

    server.register_instance(
        RopeInterface(project))

    server.serve_forever()


def main():
    import argparse

    parser = argparse.ArgumentParser(
        description='Run a traad server.')

    parser.add_argument(
        '-p, --port', metavar='N', type=int,
        dest='port', default=6942,
        help='the port on which the server will listen')

    parser.add_argument(
        '-V, --verbose',
        dest='verbose', default=False, action='store_true',
        help='print debug information')

    parser.add_argument(
        'project', metavar='P', type=str,
        help='the directory containing the project to server')

    args = parser.parse_args()

    # Configure logging
    level = logging.INFO
    if args.verbose:
        level = logging.DEBUG
    logging.basicConfig(
        level=level)

    run_server(args.port, args.project)

if __name__ == '__main__':
    main()
