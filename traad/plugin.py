from .compat import getargspec
from .rope.workspace import Workspace


class TraadPlugin:
    """Bottle plugin that manages the Workspace context for traad.

    If this plugin is active, it passes a `Context` object to the `context`
    keyword on handlers. This object has these attributes:

     - workspace: A `traad.rope.Workspace`

    """
    name = 'traad'
    api = 2

    class Context:
        def __init__(self, path):
            self.workspace = Workspace(path)

    def __init__(self, root_project_path, keyword='context'):
        self.context = TraadPlugin.Context(root_project_path)
        self.keyword = keyword

    def apply(self, callback, context):
        # Test if the original callback accepts a 'context' keyword.
        # Ignore it if it does not need a database handle.
        args = getargspec(context.callback)[0]
        if self.keyword not in args:
            return callback

        def wrapper(*args, **kwargs):
            kwargs[self.keyword] = self.context
            return callback(*args, **kwargs)

        # Replace the route callback with the wrapped one.
        return wrapper
