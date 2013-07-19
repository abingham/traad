import itertools
import logging
import sys
import traceback

import decorator

try:
    import reprlib
    repr = reprlib.repr
except ImportError:
    pass

log = logging.getLogger('traad.trace')

@decorator.decorator
def trace(f, *args, **kw):
    '''A simple tracing decorator, mostly to help with debugging.
    '''

    log.info('{}({})'.format(
        f.__name__,
        ', '.join(
            map(repr,
                itertools.chain(
                    args,
                    kw.values())))))

    try:
        return f(*args, **kw)
    except:
        einfo = sys.exc_info()
        log.error('Exception in {}: {}'.format(
            f.__name__,
            ''.join(traceback.format_exception(einfo[0], einfo[1], einfo[2]))))
        raise
