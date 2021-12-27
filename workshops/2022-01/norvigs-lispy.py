# -*- coding: utf-8 -*-
# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.13.4
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# # Norvig's _lis.py_
#
#
# ![Norvig's lispy](lispy.png)

# Contents:
#
# * [Introduction](#Introduction)
# * [Scheme Syntax](#Scheme-Syntax)
# * [Imports and types](#Imports-and-types)
# * [The Parser](#The-Parser)
#   * [Exercise 0](#Exercise-0)
# * [The Built-in Environment](#The-Built-in-Environment)
# * [A Calculator](#A-Calculator)
# * [Non-interactive execution](#Non-interactive-execution)
#   * [Exercise 1](#Exercise-1)
#   * [Exercise 2](#Exercise-2)
# * [User defined procedures](#User-defined-procedures)
# * [A more complete environment](#A-more-complete-environment)
# * [`Procedure`: a class to represent a closure](#Procedure:-a-class-to-represent-a-closure)
# * [Evaluate with `lambda`, `if`, and `quote`](#Evaluate-with-lambda,-if,-and-quote)
# * [The REPL](#The-REPL)
# * [Examples](#Examples)
# * [Syntactic sugar](#Syntactic-sugar)
#
# > **LICENSES**:<br>
#   Code © 2010-2018 Peter Norvig, [MIT License](https://github.com/fluentpython/lispy/blob/main/LICENSE)<br>
#   Text © 2022 Luciano Ramalho, [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/)
#

# ## Introduction
#
# [Peter Norvig](https://norvig.com/) of Stanford University wrote
# [_lis.py_](https://github.com/norvig/pytudes/blob/main/py/lis.py):
# an interpreter for a subset of the Scheme dialect of Lisp in 132 lines of readable Python code.
#
# Why should you study _lis.py_? This is what I got out of it:
#
# * Learning how an interpreter works gave me a deeper understanding of Python and programming languages in general—interpreted or compiled.
#
# * The simplicity of Scheme is a master class of language design.
#
# * _lis.py_ is a beautiful example of idiomatic Python code.
#
# Norvig describes _lis.py_ in a post titled 
# [(How to Write a (Lisp) Interpreter (in Python))](https://norvig.com/lispy.html). Highly recommended.
#
# Before looking at the Python code, let’s get a little taste of Scheme—in case you haven’t seen it (or Lisp) before.

# ## Scheme Syntax
#
#
# Everything in Scheme is an expression.
# There are no infix operators:
# all expressions use prefix notation like `(+ x 13)` instead of `x + 13`.
# The same prefix notation is used for function calls—e.g. `(gcd x 13)`—and
# special forms—e.g. `(define x 13)`,
# which we'd write as the assignment statement `x = 13` in
# Python.
#
# The parenthesized prefix notation is known as _S-expression_ syntax.
# Lisp and Clojure use the same syntax.
#
# Here is a simple example in Scheme:
#
# ```lisp
# (define (mod m n)
#     (- m (* n (quotient m n))))
#
# (define (gcd m n)
#     (if (= n 0)
#         m
#         (gcd n (mod m n))))
#
# (display (gcd 18 45))
# ```

# Here is the same algorithm in Python:

# +
def mod(m, n):
    return m - (m // n * n)

def gcd(m, n):
    if n == 0:
        return m
    else:
        return gcd(n, mod(m, n))

print(gcd(18, 45))
# -

# > **TIP**: Click on the cell above to select it, then hit `【CTRL】【ENTER】` to execute it.
# <br>The result will appear below the cell.
#
# Scheme has no iterative control flow commands like `while` or `for`.
# Iteration is done with recursion.
# Note how there are no assignments in the Scheme and Python examples.
# Extensive use of recursion and minimal use of assignment
# are hallmarks of programming in a functional style.
#
# In idiomatic Python I'd use the `%` operator instead of reinventing `mod`,
# and it would be more efficient to use a `while` loop instead of recursion.
# But I wanted to show two function definitions,
# and make the examples as similar as possible,
# to help you read the Scheme code.
#
#
# Now let's review the code of the Python 3.7 version of _lis.py_.
# The complete source code with tests for Python 3.10 is in the [18-with-match/lispy/py3.10/](https://github.com/fluentpython/example-code-2e/tree/master/18-with-match/lispy/py3.10/)
# directory of the Github repository
# [fluentpython/example-code-2e](https://github.com/fluentpython/example-code-2e).

#
# This notebook uses Python 3.7 to run on [Binder](https://mybinder.org/). The next code cell checks that you're using Python 3.7 or later.
#

#
# > **TIP**: Click on the cell below to select it, then hit `【SHIFT】【ENTER】` to execute and select the next cell.<br> Use `【CTRL】【ENTER】` to execute a cell and keep it selected.<br>Use these commands to execute cells as you follow along.

import sys
assert sys.version_info >= (3, 7), f'Expected Python ≥ 3.7; found: {sys.version}'
sys.version

# ## Imports and types
#
# Norvig's code does not use type hints. I added them, and made a few other small changes.
# For Python 3.7 we need to import some types from `typing`.
#

# +
################ lis.py: Scheme Interpreter in Python 3.10
## (c) Peter Norvig, 2010-18; See http://norvig.com/lispy.html
## Type hints and minor additions by Luciano Ramalho

import math
import operator as op
from collections import ChainMap
from itertools import chain
from typing import Any, NoReturn
from typing import Union, List, MutableMapping, Optional, Iterator

Symbol = str
Atom = Union[float, int, Symbol]
Expression = Union[Atom, List]

Environment = MutableMapping[Symbol, object]


# -

# The types defined are:
#
# `Symbol`: Just an alias for `str`.
# In _lis.py_, `Symbol` is used for identifiers,
# there is no string data type with operations such as slicing, splitting etc.
#
# `Atom`: A simple syntactic element such as a number or a `Symbol`—as opposed to a composite structure made of distinct parts, like a list.
#
# `Expression`: The building blocks of Scheme programs are expressions made of atoms and lists, possibly nested.
#
# > **NOTE**: Norvig's second interpreter,
# [`lispy.py`](https://github.com/fluentpython/example-code-2e/blob/master/18-with-match/lispy/original/lispy.py),
# supports strings as a data type, as well as advanced features like
# syntactic macros, continuations, and proper tail calls.
# However, `lispy.py` is almost three times longer than _lis.py_—and
# harder to understand.

# ## The Parser
#
# Norvig's parser is 36 lines of code showcasing the power of Python applied to handling the
# simple recursive syntax of S-expression—without string data,
# comments, macros, and other features of standard Scheme that make parsing more complicated (these features are implemented in `lispy.py`).

# +
def parse(program: str) -> Expression:
    "Read a Scheme expression from a string."
    return read_from_tokens(tokenize(program))

def tokenize(s: str) -> List[str]:
    "Convert a string into a list of tokens."
    return s.replace('(', ' ( ').replace(')', ' ) ').split()

def read_from_tokens(tokens: List[str]) -> Expression:
    "Read an expression from a sequence of tokens."
    if len(tokens) == 0:
        raise SyntaxError('unexpected EOF while reading')
    token = tokens.pop(0)
    if '(' == token:
        exp = []
        while tokens[0] != ')':
            exp.append(read_from_tokens(tokens))
        tokens.pop(0)  # discard ')'
        return exp
    elif ')' == token:
        raise SyntaxError('unexpected )')
    else:
        return parse_atom(token)

def parse_atom(token: str) -> Atom:
    "Numbers become numbers; every other token is a symbol."
    try:
        return int(token)
    except ValueError:
        try:
            return float(token)
        except ValueError:
            return Symbol(token)


# -

# The main function of that group is `parse` which takes an S-expressions as a `str`
# and returns an `Expression` object: an `Atom` or a `list` that may contain more atoms and nested lists.
#
# The first task of a parser is lexical analysis: identify and extract the "words" of the language.
# Language nerds call the "words" _tokens_. That task is done by `tokenize`.
#
#
# Norvig uses a smart trick in `tokenize`:
# he adds spaces before and after each parenthesis in the input and then splits it,
# resulting in a list of syntactic tokens with `'('` and `')'`
# as separate tokens.
#
# This shortcut works because there are no string literals or comments in the little Scheme of _lis.py_, so every `'('` or `')'` is an expression delimiter. (Norvig's _lispy.py_ supports strings, comments and much more.)
#
# Norvig feeds the result of lexical analysis to `read_from_tokens`,
# the recursive function that assembles an `Expression`.
#
# The parsing rules for this subset of Scheme are simple:
#
# 1. A token that looks like a number is parsed as a `float` or `int`.
# 2. Anything else that is not `'('` or `')'` is parsed as a `Symbol`—a `str` to be used as an identifier. This includes source text like `+`, `set!`, and `make-counter` that are valid identifiers in Scheme but not in Python.
# 3. Expressions inside `'('` and `')'` are recursively parsed as lists containing atoms or nested lists that may contain atoms and more nested lists.
#
# As you first study this code, I recommend you consider `read_from_tokens` a black box,
# and focus on the input and output of the high level `parse` function.
#
# Below are some examples of `parse` in action.
#
# > **TIP**:  To run the code in each cell and select the next, use `【SHIFT】【ENTER】`.<br>
# If you get `NameError: name 'parse' is not defined`, use the menu command ***Cell > Run All Above***.

parse('1.5')

parse('ni!')

parse('''
  (define double
    (lambda (n)
      (* n 2)))
''')

#
# Using the jargon of programming language theory, the output of `parse` is an **AST** (Abstract Syntax Tree):
# a convenient representation of the Scheme program as nested lists forming a tree-like structure,
# where the outermost list is the trunk, inner lists are the branches, and atoms are the leaves.
#
# This is the AST for the example `(define double (lambda (n) (* n 2)))` drawn as a tree:
#
#
# ```
#                               '*'  'n'   2
#                         'n'    └────┼────┘
#                          │          │
#            'lambda'     [ ]        [ ]
#                └─────────┼──────────┘
#                          │
# 'define'   'double'     [ ]
#     └─────────┼──────────┘
#               │
#              [ ]
# ```
#
# That tree depicts the AST `['define', 'double', ['lambda', ['n'], ['*', 'n', 2]]]`.<BR>
# Note that each element in the AST is a Python object.
#

# ### Exercise 0
#
# Replace the ellipis `...` with the AST for the given S-expressions, to make each comparison `True`.
# To run the code in the cell, hit `【CTRL】【ENTER】`. 

parse('9') == ...

parse('x/y') == ...

parse('(+ 3 7)') == ...

parse('(* c (/ 9 5))') == ...

parse('(+ 32 (* (/ 9 5) c ))') == ...


# ## Built-in functions for arithmetic
#
#
# To be useful, a programming language must have pre-defined functions, ready to use,
# like the `__builtins__` module in Python.
#
# In _lis.py_, the `standard_env` function builds and returns an `Environment` mapping loaded
# with predefined functions. Next is a very simplified version of `standard_env`; we'll add more functions later. 

def standard_env() -> Environment:
    "An environment for an s-expression calculator."
    env: Environment = {}
    env.update(vars(math))   # sin, cos, sqrt, pi, ...
    env.update(
        {
            '+': op.add,
            '-': op.sub,
            '*': op.mul,
            '/': op.truediv,
            'quotient': op.floordiv,
            'abs': abs,
            'max': max,
            'min': min,
            'round': round,
            'begin': lambda *x: x[-1],
        }
    )
    return env


# The `env` mapping is loaded with:
#
# * all functions from Python's `math` module;
# * selected operators from Python's `op` module;
# * Python built-ins directly mapped like `abs` and `round`.
#
# The `begin` function is a way to compute several expressions.
# We'll see an example using it.

# ## A Calculator
#
# The central function of the interpreter is `evaluate`.
# We'll start with a simple version that only handles 
# expressions with built-in functions and user-defined variables.
#
# > **NOTE**: Norvig's parser is simple and solid, but his evaluator is simple and fragile. He ommited error checking to keep the logic easy to follow. In his words: "Lispy does not attempt to detect, reasonably report, or recover from errors. Lispy expects the programmer to be perfect." ([source](https://norvig.com/lispy.html)).
#

def evaluate(x: Expression, env: Environment) -> Any:
    "Evaluate an expression in an environment."
    if isinstance(x, Symbol):                    # variable reference
        return env[x]
    elif not isinstance(x, list):                # constant literal
        return x
    elif x[0] == 'define':                       # (define var exp)
        _, var, exp = x
        env[var] = evaluate(exp, env)
    else:                                        # (proc arg...)
        proc_exp, *args = x
        proc = evaluate(proc_exp, env)
        arg_values = [evaluate(exp, env) for exp in args]
        return proc(*arg_values)


# Run these examples to see `evaluate` in action.
#
# A curious square:

evaluate(parse('(* 11111 11111)'), standard_env())

# If there are 876 candidates, and 123 were approved, what percentage was approved?

evaluate(parse('(* (/ 123 876) 100)'), standard_env())

# Now let's study each part of the `if/elif/…` in `evaluate`.

# ### Evaluate symbol
#
# ```python
#     if isinstance(x, Symbol):
#         return env[x]
# ```
#
# If the expression is a `Symbol`, then look it up in the environment.
#
#

evaluate('pi', standard_env())

evaluate('+', standard_env())

# ### Evaluate other atoms
#
# ```python
#     elif not isinstance(x, list):
#         return x
# ```
#
# If the expression is not a `list` and not a `Symbol` (because of the preceding check), then assume it is a constant literal and return it as is.

evaluate(1.5, standard_env())

# ### Evaluate `(define var exp)`
#
# ```python
#     elif x[0] == 'define':
#         _, var, exp = x
#         env[var] = evaluate(exp, env)
# ```
#
# If the expression is a `list` starting with the keyword `define`, then it should be followed by a `Symbol` and an `Expression`. Recursively evaluate the expression in environment, and store it in `env` using the `Symbol` as key.

env = standard_env()
evaluate(parse('(define answer (* 7 6))'), env)
env['answer']

# ### Evaluate function call `(proc arg…)`
#
# ```python
#     else:
#         proc_exp, *args = x
#         proc = evaluate(proc_exp, env)
#         arg_values = [evaluate(exp, env) for exp in args]
#         return proc(*arg_values)
# ```
#
# If the expression is a `list` that does not start with a keyword, then:
#
# 1. Evaluate the first expression—it should return a procedure (a.k.a. function).
# 2. Evaluate the remaining expressions (the argument values)
# 3. Call the procedure with the argument values.

evaluate(['quotient', 8, 3], standard_env())

evaluate(['*', ['/', 123, 876], 100], standard_env())

# `evaluate` can process deeply nested expressions, but only one expression at the top level. To bundle several expressions into one, use the `(begin ...)` function. All the arguments given to `begin` are evaluated before `begin` is called, and `begin` returns the value of the last argument. For example:

env = standard_env()
percent = """
(begin
  (define a 126)
  (define b (* 6 50))
  (* (/ a b) 100)
)
"""
evaluate(parse(percent), env)

# After the previous code, `env` now holds two variables: `a` and `b`.

env['a'], env['b']


# To evaluate a sequence of S-expressions as a program, we use the `run` function, described next.

# ## Non-interactive execution with `run()`
#
# The following functions take Scheme source code as a string and execute it.

# +
def run_lines(source: str, env: Optional[Environment] = None) -> Iterator[Any]:
    global_env: Environment = ChainMap({}, standard_env())
    if env is not None:
        global_env.update(env)
    tokens = tokenize(source)
    while tokens:
        exp = read_from_tokens(tokens)
        yield evaluate(exp, global_env)


def run(source: str, env: Optional[Environment] = None) -> Any:
    for result in run_lines(source, env):
        pass
    return result


# -

# With `run()` we don't need `(begin …)` to evaluate several expressions—but `begin` is still useful in other situations.

percent = """
(define a 126)
(define b (* 6 50))
(* (/ a b) 100)
"""
run(percent)

# ### Exercise 1
#
# This is the formula to convert temperatures from Celsius to Fahrenheit:
#
# `f = (9 / 5) * c + 32`
#
# Here is the code to convert 20°C to °F:

c_to_f = """
(define c 20)
(+ 32 (* c (/ 9 5)))
"""
run(c_to_f)

# The inverse conversion function is:
#
# `c = (f − 32) * 5 / 9`
#
# In the code below, replace `(+ 1 2)` with the expression to convert 68°F to °C.

f_to_c = """
(define f 68)
(+ 1 2)
"""
run(f_to_c)

# ### Exercise 2
#
# Python's `math` module includes a `factorial` function, which is part of the environment returned by `standard_env`:

run('(factorial 10)')

# Scheme accepts `!` as an identifier. Your task is to make Python's `factorial` available through the `!` symbol in Scheme. Follow the steps.
#
# **Step 2.1** Uncomment the expression below and run it to see an error. Look at the last line of the error output. What is the error? Do you understand why that is the error?

# +
# run('(! 10)') == 3628800
# -

# **Step 2.2.** Edit the `standard_env` function above to add an entry for `!`,
# mapping to Python's `math.factorial` function.
#
# **Step 2.3.** Run the expression above to confirm that the result is `True`.
#

# ### Exercise 3
#
# In standard Scheme, the `+` operator is variadic, i.e. it accepts any number of arguments.
# With 0 arguments, `(+)` returns `0`; otherwise it returns the sum of all arguments.
#
# **Step 3.1.** Uncomment the expression below and run it to see an error. Does the error make sense to you?

# +
# run('(= 10 (+ 1 2 3 4))')
# -

# **Step 3.2.** Edit the `standard_env` function above to re-implement `+` to make the expression above return `True`.
#
# > **HINT 3.2.1**: Hidden below is the source code for a variadic version of Python's `sum()`. Consider doing the exercise without revealing the hint. To reveal the code, uncomment the `print()` line and run the cell. 

from base64 import b64decode
blob = (b'ZGVmIHZhcmlhZGljX3N1bSgqYXJncyk6CiAgICByZXR1cm4g'
        b'c3VtKGFyZ3MpCgp2YXJpYWRpY19zdW0oMSwgMiwgMywgNCk=')
# print(b64decode(blob).decode('utf8'))

# > **HINT 3.2.2**: the same function in a single line of Python. Try to do the exercise without revealing the hint. To reveal it, uncomment the `print()` line and run the cell. 

blob = b'ZiA9IGxhbWJkYSAqYXJnczogc3VtKGFyZ3MpCmYoMSwgMiwgMywgNCk='
# print(b64decode(blob).decode('utf8'))

# ## A more complete environment

# This new version of `standard_env()` defines functions that handle lists, comparisons, and more.

def standard_env() -> Environment:
    "An environment with some Scheme standard procedures."
    env: Environment = {}
    env.update(vars(math))   # sin, cos, sqrt, pi, ...
    env.update(
        {
            '+': op.add,
            '-': op.sub,
            '*': op.mul,
            '/': op.truediv,
            'quotient': op.floordiv,
            '>': op.gt,
            '<': op.lt,
            '>=': op.ge,
            '<=': op.le,
            '=': op.eq,
            'abs': abs,
            'append': lambda *args: list(chain(*args)),          
            'apply': lambda proc, args: proc(*args),
            'begin': lambda *x: x[-1],
            'car': lambda x: x[0],
            'cdr': lambda x: x[1:],
            'cons': lambda x, y: [x] + y,
            'eq?': op.is_,
            'equal?': op.eq,
            'filter': lambda *args: list(filter(*args)),
            'length': len,
            'list': lambda *x: list(x),
            'list?': lambda x: isinstance(x, list),
            'map': lambda *args: list(map(*args)),
            'max': max,
            'min': min,
            'not': op.not_,
            'null?': lambda x: x == [],
            'number?': lambda x: isinstance(x, (int, float)),
            'procedure?': callable,
            'round': round,
            'symbol?': lambda x: isinstance(x, Symbol),
            'display': lambda x: print(lispstr(x), end=''),
            'newline': lambda: print(),
        }
    )
    return env


# This `standard_env` is enhanced with:
#
# * more Python built-ins renamed like `len` as `length` and `callable` as `procedure?`;
# * simple but powerful functions built with Python's `lambda`.
#
# The functions with a `?` suffix (like `list?`, `null?`, `number?` etc.) are _predicates_:<BR>single argument functions that check the type of the argument and return a boolean.
#
# The `(display …)` function shows results, but does not emit a newline.
# For that, ywe need to call `(newline)` (just like that, without arguments).
#
# The call `(display s)` calls `lispstr(s)` which is the inverse function of `parse`:
# given a Python object representing an expression as an AST,
# `lispstr` returns the its Scheme source code as a string.

def lispstr(exp: Expression) -> str:
    "Convert a Python object back into a Lisp-readable string."
    if isinstance(exp, list):
        return '(' + ' '.join(map(lispstr, exp)) + ')'
    else:
        return str(exp)


# For example:

lispstr(['+', 32, ['*', ['/', 9, 5], 'c']])


# ## `Procedure`: a class to represent a closure
#
# The next improvement to `evaluate()` will include the `(lambda …)` form to allow user-defined functions (the authors of Scheme prefer the term *procedure*). To support `lambda`, we need a class to represent a procedure:

class Procedure:
    "A user-defined Scheme procedure."

    def __init__(self, parms: List[Symbol], body: Expression, env: Environment):
        self.parms = parms
        self.body = body
        self.env = env

    def __call__(self, *args: Expression) -> Any:
        local_env = dict(zip(self.parms, args))
        env: Environment = ChainMap(local_env, self.env)
        return evaluate(self.body, env)


# The `Procedure` class could very well be named `Closure`,
# because that's what it represents:
# a function definition together with an environment captured when the function is defined.
# The required parameters to create a `Procedure` are:
#
# `parms`: the function parameter names as a list of symbols. This list may be empty.
#
# `body`: the body of the function as an expression to be evaluated when the function is invoked.
#
# `env`: the environment where the function is created. This is what makes it a [*closure*](https://en.wikipedia.org/wiki/Closure_(computer_programming)).
#
# The `__init__` method simply stores the arguments passed. None of them is evaluated when the function is defined.
#
# The environment is used when the function is called to provide the values of
# the [*non-local variables*](https://en.wikipedia.org/wiki/Non-local_variable):
# variables that appear in the body of the function but that
# are not parameters or local variables.
#
# Let's create a `Procedure` "by hand" to see how it works:

double = Procedure(['n'], ['*', 'n', 2], standard_env())
double(4)


# ## Evaluate with `lambda`, `if`, and `quote`
#
# To transform the calculator into a worthy subset of Scheme, we need to support user defined functions, conditionals and the `(quote …)` form to handle S-expressions as data—instead of evaluating them.
#

def evaluate(x: Expression, env: Environment) -> Any:
    "Evaluate an expression in an environment."
    if isinstance(x, str):                       # variable reference
        return env[x]
    elif not isinstance(x, list):                # constant literal
        return x
    elif x[0] == 'define':                       # (define var exp)
        _, var, exp = x
        env[var] = evaluate(exp, env)
    elif x[0] == 'lambda':                       # (lambda (var...) body)
        _, parms, body = x
        return Procedure(parms, body, env)
    elif x[0] == 'quote':                        # (quote exp)
        _, exp = x
        return exp
    elif x[0] == 'if':                           # (if test consequence alternative)
        _, test, consequence, alternative = x
        if evaluate(test, env):
            return evaluate(consequence, env)
        else:
            return evaluate(alternative, env)
    else:                                        # (proc arg...)
        proc_exp, *args = x
        proc = evaluate(proc_exp, env)
        arg_values = [evaluate(exp, env) for exp in args]
        return proc(*arg_values)


# ### Evaluate `(lambda (var…) body)`
#
# ```python
#     elif x[0] == 'lambda':                       
#         _, parms, body = x
#         return Procedure(parms, body, env)
# ```
#
# If the expression is a `list` starting with the keyword `lambda`, followed by a list of 0 or more symbols, and a single `body` expression, then build and return a `Procedure`.
#
# Example:

percent = run('(lambda (a b) (* (/ a b) 100))')
percent(15, 20)

# The result of `(lambda …)` is an anonymous function, so it is not stored in the environment. To create a named function, use `lambda` with `define`.
#
# > **NOTE**: This version of _lis.py_ only accepts a single expression as the `body` of the function. Use `(begin …)` to wrap multiple expressions. The result of the function will be the value of the last expression.

# ### Evaluate `(quote exp)`
#
# ```python
#     elif x[0] == 'quote':
#         _, exp = x
#         return exp
# ```
#
# If the expression is a `list` starting with the keyword `quote` followed by a single expression, return the expression without evaluating it.
#
# Examples:

run('(quote no-such-name)')  # undefined symbol, would raise error if evaluated

run('(quote (99 bottles of beer))')  # 99 is not the name of a function or reserved word

run('(quote (/ 10 0))')  # this would raise division by zero if evaluated

# ### Evaluate `(if test consequence alternative)`
#
# ```python
#     elif x[0] == 'if':
#         _, test, consequence, alternative = x
#         if evaluate(test, env):
#             return evaluate(consequence, env)
#         else:
#             return evaluate(alternative, env)
# ```
#
# If the expression is a `list` starting with the keyword `if`, followed by exactly three expressions, evaluate `test`. If true, evaluate `consequence`; otherwise evaluate `alternative`.
#
# Example:

run('(if (= 3 3) 1 0)')

run('(if (= 3 30) 1 0)')

source = '''
(define pass-fail
    (lambda (grade)
        (if (>= grade 5)
            (quote PASS)
            (quote FAIL))))
(pass-fail 7)
'''
run(source)


# ## The REPL
#
# Norvig's REPL (Read-Eval-Print-Loop) is easy to undersand but not user-friendly.
# If no command-line arguments are given to _lis.py_,
# the `repl()` function is invoked by `main()`—defined at the end of the module.
# At the `lis.py>` prompt we must enter correct and complete expressions—if
# we forget to close one parenthesis, _lis.py_ crashes.
#
# > **TIP**: `repl()` does not work well inside a Jupyter notebook.
# It's better to try it out exporting this notebook as a script _norvigs-lispy.py_
# and then running it from the shell:<BR>
# `$ python3 norvigs-lispy.py`

# +
def repl(prompt: str = 'lis.py> ') -> NoReturn:
    "A prompt-read-evaluate-print loop."
    global_env: Environment = standard_env()
    while True:
        val = evaluate(parse(input(prompt)), global_env)
        if val is not None:
            print(lispstr(val))


def lispstr(exp: object) -> str:
    "Convert a Python object back into a Lisp-readable string."
    if isinstance(exp, list):
        return '(' + ' '.join(map(lispstr, exp)) + ')'
    else:
        return str(exp)


# -

# Function `repl` calls `standard_env()` to provide built-in functions for the global environment,
# then enters an infinite loop reading and parsing each input line,
# evaluating it in the global environment and displaying the result—unless it's `None`.
# The `global_env` may be modified by `evaluate`.
#
# > **NOTE**: As I studied Norvig's _lis.py_ and _lispy.py_, I started a fork named
#   [`mylis`](https://github.com/fluentpython/lispy/blob/main/mylis)
#   which adds some features, including a REPL that accepts partial S-expressions
#   and prompts for the continuation, similar to how Python's REPL
#   knows we are not finished and presents the secondary prompt `...` until
#   we enter a complete expression or statement that can be evaluated.
#   `mylis` also handles a few errors gracefully, but it's still easy to crash.

# ## Examples
#

# ### Greatest common divisor
#
# The [Euclidean algorihm](https://en.wikipedia.org/wiki/Euclidean_algorithm).
#
# > **NOTE**: This is example uses `lambda` inside `define` instead of the shortcut `define` form listed in 
# [Scheme Syntax](#Scheme-Syntax), which creates named procedures directly.
# We'll get back to the `define` shortcut in the last section of the tutorial, [Syntactic Sugar](Syntactic-Sugar)

gcd_src = '''
(define mod (lambda (m n)
    (- m (* n (quotient m n)))))

(define gcd (lambda (m n)
    (if (= n 0)
        m
        (gcd n (mod m n)))))

(gcd 18 45)
'''
run(gcd_src)

#
# ### Simple recursive factorial

fact_src = '''
(define ! (lambda (n)
    (if (< n 2) 
        1 
        (* n (! (- n 1)))
)))

(! 5)
'''
run(fact_src)

# ### Tail-recursive factorial
#
# The `factorial-iter` function is tail-recursive: the recursive call is returned as the result. That's a
# [*tail call*](https://en.wikipedia.org/wiki/Tail_call). Scheme can evaluate tail recursion without growing te call stack.
#
# In contrast, the [Simple recursive factorial](#Simple-recursive-factorial) is not tail recursive: the result of the recursive call is multiplied by `n` before it is returned.

fact_src = '''
(define ! (lambda (n)
    (factorial-iter n 1)))

(define factorial-iter
    (lambda (n product)
        (if (= n 1)
            product
            (factorial-iter (- n 1) (* n product))
        )
    )
)
      
(! 5)
'''
run(fact_src)

# > **NOTE**: _lis.py_ does not implement proper tail calls (PTC)—a.k.a. tail call optimization (TCO).
# Therefore, there is no advantage in writing tail recursive functions. But
# [`lispy.py`](https://github.com/norvig/pytudes/blob/main/py/lispy.py) and
# [`mylis_2`](https://github.com/fluentpython/lispy/blob/main/mylis/mylis_2/lis.py) implement PTC, so tail-recursion does not grow the stack, and tail-recursive code is more efficient.

# ### Square root algorithm
#
# This is known as the [Babylonian method](https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method),
# an ancient special case of [Newton's method](https://en.wikipedia.org/wiki/Newton%27s_method).
# This code is adapted from an [example](https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node12.html) in the book *Structure and Interpretation of Computer Programs*.

sqrt_src = """
(define sqrt (lambda (x)
    (sqrt-iter 1.0 x)))
    
(define sqrt-iter (lambda (guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x))))
        
(define good-enough? (lambda (guess x)
    (< (abs (- (* guess guess) x)) 0.001)))
    
(define improve (lambda (guess x)
    (average guess (/ x guess))))
    
(define average (lambda (x y)
    (/ (+ x y) 2)))
    
(sqrt 123454321)
"""
run(sqrt_src)

# ### Average of a list of numbers
#
# A simple example using the list handling functions `null?`, `car`, `cdr`, and `list`.

average_scm = """
(define average (lambda (numbers)
    (average-iter numbers 0 0)))

(define average-iter (lambda (numbers count sum)
    (if (null? numbers)
        (/ sum count)
        (average-iter (cdr numbers) (+ count 1) (+ sum (car numbers))))))

(average (list 1 2 3 4))
"""
run(average_scm)

# ### Quicksort
#
# Tony Hoare's elegant [recursive sorting algorithm](https://en.wikipedia.org/wiki/Quicksort).
#
# Note that Scheme's `append` is very different from Python's `.append` method. 
# The definition of `append` in `standard_env` builds a new list by concatenating lists.
# So `(append l1 l2)` in Scheme is like `l1 + l2` in Python.
#
# `filter` is a high-order function in Scheme and in Python: it takes a predicate and a list as arguments, and returns a new list with the items that make the predicate true.

quicksort_src = """
(define quicksort (lambda (lst)
    (if (null? lst)
        lst
        (begin
            (define pivot (car lst))
            (define rest (cdr lst))
            (append
                (quicksort
                    (filter (lambda (x) (< x pivot)) rest))
                (list pivot)
                (quicksort
                    (filter (lambda (x) (>= x pivot)) rest)))
))))
(display (quicksort (list 2 1 6 3 4 0 8 9 7 5)))
(newline)
"""
run(quicksort_src)

# > **NOTE**: the code above works in *lis.py* but not in standard Scheme.
# The form `define` cannot be used in that context in standard Scheme;
# instead, we would use the form `let`—which doesn't exist in *lis.py*.
# The implementation of `define` in *lis.py* is a simplification by Norvig,
# mimicking assignment semantics in Python.
# When `define` occurs at the global level, a variable is created or updated in the global environment.
# In the body of a function, `define` creates or updates a variable
# in the first dictionary in `ChainMap`, which holds the local environment. 

# ## Syntactic sugar
#
# Standard Scheme provides an alternative syntax for `define` that allows defining named functions without `lambda`.
#
# The syntax is: `(define (name parms…) body…)`, where:
#
# `name`: the name of the function to be defined (a `Symbol`);
#
# `parms…`: 0 or more symbols declaring the parameter names;
#
# `body…`: 1 or more expressions to be used as the body of the function.
#
# This is an example of _syntactic sugar_: new syntax that does not add any functionality to the language, but makes it more convenient to use.
#
# The version of `gcd` shown in [Scheme Syntax](#Scheme-Syntax) uses that shortcut syntax. Here it is again:
#
# ```lisp
# (define (mod m n)
#     (- m (* n (quotient m n))))
#
# (define (gcd m n)
#     (if (= n 0)
#         m
#         (gcd n (mod m n))))
#
# (gcd 18 45)
# ```
#
# ### Final exercise
#
# Check your understanding of _lis.py_ by implementing the shortcut syntax of `define` in the `evaluate()` function.
# Test your work by running the example below. The result should be 9.
#

gcd2_src = '''
(define (mod m n)
    (- m (* n (quotient m n))))

(define (gcd m n)
    (if (= n 0)
        m
        (gcd n (mod m n))))

(gcd 18 45)
'''
# run(gcd2_src)

# ### The End
#
# Congratulations, you've just studied and changed a working interpreter for a
# [Turing-complete](https://en.wikipedia.org/wiki/Turing_completeness) subset of Scheme!
