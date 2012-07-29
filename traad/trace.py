import decorator, logging


log = logging.getLogger('traad.trace')

@decorator.decorator
def trace(f, *args, **kw):
    '''A simple tracing decorator, mostly to help with debugging.
    '''
    log.debug("calling %s with args %s, %s" % (f.__name__, args, kw))
    return f(*args, **kw)