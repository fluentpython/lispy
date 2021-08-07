#!/usr/bin/env python3

################ lis.py: Scheme Interpreter in Python 3.10
## (c) Peter Norvig, 2010-18; See http://norvig.com/lispy.html
## Minor edits for Fluent Python, Second Edition (O'Reilly, 2021)
## by Luciano Ramalho, adding type hints and pattern matching.

################ imports and types
import math
import operator as op
import readline  # this import enables readline for input()
from collections import ChainMap
from collections.abc import MutableMapping
from itertools import chain
from typing import Any, TypeAlias

from exceptions import UnexpectedCloseParen, UnexpectedEndOfSource, UndefinedSymbol

Symbol: TypeAlias = str
Atom: TypeAlias = int | float | Symbol
Expression: TypeAlias = Atom | list

Environment: TypeAlias = MutableMapping[Symbol, object]


class Procedure:
    "A user-defined Scheme procedure."

    def __init__(
        self, parms: list[Symbol], body: list[Expression], env: Environment
    ):
        self.parms = parms
        self.body = body
        self.env = env

    def __call__(self, *args: Expression) -> Any:
        local_env = dict(zip(self.parms, args))
        env: Environment = ChainMap(local_env, self.env)
        for exp in self.body:
            result = evaluate(exp, env)
        return result



################ global environment

def display(exp: object) -> str:
    output = lispstr(exp)
    print(output)
    return output

def standard_env() -> Environment:
    "An environment with some Scheme standard procedures."
    env: Environment = {}
    env.update(vars(math))   # sin, cos, sqrt, pi, ...
    env.update({
            '+': lambda *args: sum(args),
            '-': op.sub,
            '*': op.mul,
            '/': op.truediv,
            '//': op.floordiv,
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
            'display': display,
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
    })
    return env


################ parse, tokenize, and read_from_tokens


def parse(program: str) -> Expression:
    "Read a Scheme expression from a string."
    return read_from_tokens(tokenize(program))


def tokenize(s: str) -> list[str]:
    "Convert a string into a list of tokens."
    return s.replace('(', ' ( ').replace(')', ' ) ').split()


def read_from_tokens(tokens: list[str]) -> Expression:
    "Read an expression from a sequence of tokens."
    if len(tokens) == 0:
        raise UnexpectedEndOfSource()
    token = tokens.pop(0)
    if '(' == token:
        exp = []
        while tokens and tokens[0] != ')':
            exp.append(read_from_tokens(tokens))
        if not tokens:
            raise UnexpectedEndOfSource()
        tokens.pop(0)  # discard ')'
        return exp
    elif ')' == token:
        raise UnexpectedCloseParen()
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


################ interaction: a REPL


def repl(prompt: str = 'mylis.py> ') -> None:
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


################ eval

KEYWORDS = ['quote', 'if', 'define', 'lambda']

def evaluate(exp: Expression, env: Environment) -> Any:
    "Evaluate an expression in an environment."
    match exp:
        case int(x) | float(x):                             # number literal
            return x
        case Symbol(var):                                   # variable reference
            try:
                return env[var]
            except KeyError as exc:
                raise UndefinedSymbol(var) from exc
        case []:                                            # empty list
            return []
        case ['quote', exp]:                                # (quote exp)
            return exp
        case ['if', test, consequence, alternative]:        # (if test consequence alternative)
            if evaluate(test, env):
                return evaluate(consequence, env)
            else:
                return evaluate(alternative, env)
        case ['define', Symbol(var), value_exp]:            # (define var exp)
            env[var] = evaluate(value_exp, env)
        case ['define', [Symbol(name), *parms], *body       # (define (name parm...) body1 bodyN...)
              ] if len(body) > 0:
            env[name] = Procedure(parms, body, env)
        case ['lambda', [*parms], *body] if len(body) > 0:  # (lambda (parm...) body1 bodyN...)
            return Procedure(parms, body, env)
        case [op, *args] if op not in KEYWORDS:             # (proc arg...)
            proc = evaluate(op, env)
            values = (evaluate(arg, env) for arg in args)
            return proc(*values)
        case _:
            raise SyntaxError(lispstr(exp))