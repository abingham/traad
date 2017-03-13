from .foo import Foo

class Bar:
    def __init__(self):
        self.foo = Foo()

    def some_func(self):
        return 1

b = Bar()
b.some_func()

def a_free_func(a, c):
    return a + b + c
