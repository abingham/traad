import logging

import decorator

from traad.rope.log import log

@decorator.decorator
def validate(f, self, *args, **kwargs):
    "Call self.proj.validate() before running the function."
    log.info('Validating')
    self.proj.validate()
    return f(self, *args, **kwargs)
