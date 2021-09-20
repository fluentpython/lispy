# lis.py: Python 2.7 edition

The _lis.py_ interpreter in this directory is the one described in (and linked from) Peter Norvig's post
[_(How to Write a (Lisp) Interpreter (in Python))_](https://norvig.com/lispy.html).

This version of _lis.py_ requires Python 2.7 because it uses `raw_input` (now named `input`) and `apply` which was removed in Python 3 as it became redundant with the evolution of Python's syntax
to allow `function(*args, **kwargs)` instead of `apply(function, args, kwargs)`.

On the other hand, the Python 2.7 _lis.py_ implements the `set!` special form which the Python 3 _lis.py_ doesn't have—but the more advanced _lispy.py_ does.
