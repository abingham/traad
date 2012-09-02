import decorator, logging


log = logging.getLogger('traad.trace')

@decorator.decorator
def trace(f, *args, **kw):
    '''A simple tracing decorator, mostly to help with debugging.
    '''
    log.info('{}({}, {})'.format(
        f.__name__,
        args,
        kw))
    return f(*args, **kw)