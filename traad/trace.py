import decorator, itertools, logging


log = logging.getLogger('traad.trace')

@decorator.decorator
def trace(f, *args, **kw):
    '''A simple tracing decorator, mostly to help with debugging.
    '''

    def short_repr(x, max_length=200):
        r = repr(x)
        if len(r) > max_length:
            r = r[:max_length - 3] + '...'
        return r

    log.info('{}({})'.format(
        f.__name__,
        ', '.join(
            map(short_repr,
                itertools.chain(
                    args,
                    kw.values())))))

    try:
        return f(*args, **kw)
    except:
        log.exception('Exception in {}'.format(f.__name__))
        raise