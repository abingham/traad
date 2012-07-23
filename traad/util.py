import collections

def consume(iterator, n=None):
    '''Consume up to ``n`` elements from ``iterator``.
    '''
    if n is None:
        collections.deque(iterator, maxlen=0)
    else:
        next(islice(iterator, n, n), None)

def cmap(func, seq):
    '''A "consuming" map.

    Calls ``func(s)`` for each element in ``seq``.

    The return values are discarded.
    '''
    consume(map(func, seq))