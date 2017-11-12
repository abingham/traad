from functools import wraps


def validate(func):
    """Decorator that calls 'validate()' on the root project before running the
function."""
    @wraps(func)
    def wrapper(self, *args, **kwargs):
        self.root_project.validate()
        return func(self, *args, **kwargs)
    return wrapper
