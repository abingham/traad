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
    print("Listening on http://localhost:{}/".format(app))


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
    app.on_startup.append(print_port)

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

    # web.run_app(app, host=host, port=args.port)
    loop = asyncio.get_event_loop()
    handler = app.make_handler()
    coroutine = loop.create_server(handler, '0.0.0.0', 0)
    server = loop.run_until_complete(coroutine)
    print('Listening on http://localhost:{}'.format(server.sockets[0].getsockname()[1]))

    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.close()
        loop.run_until_complete(server.wait_closed())
        loop.run_until_complete(handler.finish_connections(1.0))
        loop.close()


if __name__ == '__main__':
    main()
