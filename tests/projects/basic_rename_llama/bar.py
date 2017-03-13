from .foo import Llama

class Bar:
    def __init__(self):
        self.foo = Llama()

    def some_func(self):
        return 1

b = Bar()
b.some_func()

def a_free_func(a,    b,

            c):
    return a + b + c
