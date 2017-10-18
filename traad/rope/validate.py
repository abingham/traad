import decorator

from traad.rope.log import log


@decorator.decorator
def validate(f, self, *args, **kwargs):
    "Call self.proj.validate() before running the function."
    log.info('Validating')
    for project in self.projects:
        project.validate()
    log.info('Done validating')
    return f(self, *args, **kwargs)
