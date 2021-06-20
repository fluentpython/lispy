################ Calculator: derived from lis.py for Python 3.10

## (c) Peter Norvig, 2010-18; See http://norvig.com/lispy.html
## adapted by Luciano Ramalho with type hints and pattern matching.

################ Imports and Types

import math
import operator as op
import random
from collections import ChainMap
from collections.abc import MutableMapping
from typing import Any, TypeAlias

Symbol: TypeAlias = str
Atom: TypeAlias = float | int | Symbol
Expression: TypeAlias = Atom | list

Environment: TypeAlias = MutableMapping[Symbol, object]


################ Global Environment


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
            '>': op.gt,
            '<': op.lt,
            '>=': op.ge,
            '<=': op.le,
            '=': op.eq,
            'random': random.random,
        }
    )
    return env


################ Parsing: parse, tokenize, and read_from_tokens


def parse(program: str) -> Expression:
    "Read a Scheme expression from a string."
    return read_from_tokens(tokenize(program))


def tokenize(s: str) -> list[str]:
    "Convert a string into a list of tokens."
    return s.replace('(', ' ( ').replace(')', ' ) ').split()


def read_from_tokens(tokens: list[str]) -> Expression:
    "Read an expression from a sequence of tokens."
    if len(tokens) == 0:
        raise SyntaxError('unexpected EOF while reading')
    token = tokens.pop(0)
    if '(' == token:
        L = []
        while tokens[0] != ')':
            L.append(read_from_tokens(tokens))
        tokens.pop(0)   # discard ')'
        return L
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


################ eval


global_env: Environment = standard_env()

def evaluate(x: Expression) -> Any:
    "Evaluate an expression in an environment."
    match x:
        case Symbol(var):                               # variable reference
            return global_env[var]
        case literal if not isinstance(x, list):        # constant literal
            return literal
        case ['if', test, conseq, alt]:                 # (if test conseq alt)
            if evaluate(test):
                return evaluate(conseq)
            else:
                return evaluate(alt)
        case ['define', Symbol(var), exp]:              # (define var exp)
            global_env[var] = evaluate(exp)
        case [op, *args]:                               # (proc arg...)
            proc = evaluate(op)
            values = (evaluate(arg) for arg in args)
            return proc(*values)


################ High level API


def calc(source: str) -> Any:
    return evaluate(parse(source))


################ Interaction: A REPL


def repl(prompt: str = 'calc> ') -> None:
    "A prompt-read-evaluate-print loop."
    while True:
        val = evaluate(parse(input(prompt)))
        if val is not None:
            print(lispstr(val))


def lispstr(exp: object) -> str:
    "Convert a Python object back into a Lisp-readable string."
    if isinstance(exp, list):
        return '(' + ' '.join(map(lispstr, exp)) + ')'
    else:
        return str(exp)


if __name__ == '__main__':
    # activate line editing and history
    import readline
    repl()
