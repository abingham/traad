import logging
import sys

import traad.app


log = logging.getLogger('traad.server')


def run_server(app, port):
    host = 'localhost'

    log.info('Python version: {}'.format(sys.version))

    log.info(
        'Running traad server at {}:{}'.format(
            host,
            port))

    app.run(host=host, port=port)


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

    args = parser.parse_args()

    # Configure logging
    level = {
        0: logging.WARNING,
        1: logging.INFO,
        2: logging.DEBUG
    }[args.verbosity]

    logging.basicConfig(
        level=level)

    with traad.app.activate_app() as app:
        run_server(app, args.port)

if __name__ == '__main__':
    main()
