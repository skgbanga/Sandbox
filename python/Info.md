General python learnings

  - How to distinguish between package and module? : Look for __file__ attribute
  - Since python 3.3+, you don't need emtpy `__init__.py` (these are called namespace packages)
  - `python -m` to execute a module
  - `__all__` specifies what things should be imported when users do from .. import *
  - `from timeit import timeit` gives a good way to know how much time something takes
  - `_` is the last result in python repl
  - `print{'{:f}'.format(x)}` to show something in non scientific notation
  - conditional expressions: z = x if True else y
  - When we need function to accept atleast one argument, define function body as
    - def volume(lenght, `*`lengths):

  - difference between `nonlocal` and `global` is which scope is searched
  - it is a syntax error for a nonlocal variable to not exist
  - nonlocal variables can be used for making some nice closure based functions
  - class instances can be used as decorators and their behaviour can be changed later in the program e.g.

  ```
  class Tracer:
    def __init__(self):
      self.enabled = False

    def __call__(self, f):
      def wrap(*args, **kwargs):
        if self.enabled:
          print('calling ', f)
        return f(*args, **kwargs)

  tracer = Tracer()
  @tracer
  def foo():
    pass
  ```

  At some later point in the program, `tracer.enabled=True` to start tracing the calls
  - zfill method in string is handy to make sure the string is right length by padding with zeroes
  - If we need polymorphic behaviour with @staticmethod, call the method with `self` instead of `cls`
  - `cls` parameter in @classmethod points to the derived class
  - There are interesting issues when one tries to override @property.setter in derived class
  - if `__str__` is not defined, `__repr__` is used. Converse is NOT true.
  - '{!r}'.format(x) will call `__repr__` of x object
  - '{!s}'.format(x) will call `__str__` of x object
