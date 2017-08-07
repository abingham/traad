import asyncio
import itertools
import logging
import sys

from aiohttp import web

from .rope.project import Project
from .routes import setup_routes
from .state import State

log = logging.getLogger('traad.app')


async def print_port(app):
    server = app['asyncio_servers'][0]
    socket = server.sockets[0]
    port = socket.getsockname()[1]
    print('Listening on http://localhost:{}/'.format(port))


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

    # TODO: Add middleware that looks for exceptions and does a 400
    app = web.Application(middlewares=[])
    app.on_pre_serve.append(print_port)

    app['task_ids'] = itertools.count()
    app['state'] = State()
    app['project'] = Project(args.project)

    setup_routes(app)

    host = 'localhost'

    log.info('Python version: {}'.format(sys.version))

    log.info(
        'Running traad server for app "{}" at {}:{}'.format(
            app,
            host,
            args.port))

    web.run_app(app, host=host, port=args.port)


if __name__ == '__main__':
    main()
