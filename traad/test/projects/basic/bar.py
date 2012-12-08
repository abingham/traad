from .foo import Foo

class Bar:
    def __init__(self):
        self.foo = Foo()

    def some_func(self):
        return 1