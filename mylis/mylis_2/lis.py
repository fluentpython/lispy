#!/usr/bin/env python3

################ lis.py: Scheme Interpreter in Python 3.10
## (c) Peter Norvig, 2010-18; See http://norvig.com/lispy.html
## Minor edits for Fluent Python, Second Edition (O'Reilly, 2021)
## by Luciano Ramalho, adding type hints and pattern matching.

################ imports and types
import math
import operator as op
from collections import ChainMap
from collections.abc import MutableMapping
from typing import Any, TypeAlias

from exceptions import (
    UnexpectedCloseParen, UnexpectedEndOfSource, UndefinedSymbol,
    InvalidSyntax, EvaluatorException,
)

Symbol: TypeAlias = str
Number: TypeAlias = int | float
Atom: TypeAlias = int | float | Symbol
Expression: TypeAlias = Atom | list

TCO_ENABLED = True


class Environment(ChainMap):
    def change(self, key: Symbol, value: object):
        for map in self.maps:
            if key in map:
                map[key] = value
                return
        raise KeyError(key)


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
        env = Environment(local_env, self.env)
        for exp in self.body:
            result = evaluate(exp, env)
        return result


################ global environment


def standard_env() -> Environment:
    "An environment with some Scheme standard procedures."
    env = Environment()
    env.update(vars(math))   # sin, cos, sqrt, pi, ...
    env.update({
        '+':op.add, '-':op.sub, '*':op.mul, '/':op.truediv,
        '>':op.gt, '<':op.lt, '>=':op.ge, '<=':op.le, '=':op.eq,
        '//': op.floordiv,  # added for Python 3 compatibility
        'abs':     abs,
        'append':  op.add,
        'apply':   lambda proc, args: proc(*args),
        'begin':   lambda *x: x[-1],
        'car':     lambda x: x[0],
        'cdr':     lambda x: x[1:],
        'cons':    lambda x,y: [x] + y,
        'eq?':     op.is_,
        'equal?':  op.eq,
        'length':  len,
        'list':    lambda *x: list(x),
        'list?':   lambda x: isinstance(x,list),
        'map':     lambda *args: list(map(*args)),
        'max':     max,
        'min':     min,
        'not':     op.not_,
        'null?':   lambda x: x == [],
        'number?': lambda x: isinstance(x, Number),
        'procedure?': callable,
        'round':   round,
        'symbol?': lambda x: isinstance(x, Symbol),
    })
    return env


################ parse, tokenize, and read_from_tokens


def parse(source: str) -> Expression:
    "Read a Scheme expression from a string."
    return read_from_tokens(tokenize(source))


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


def repl(prompt: str = 'lis.py> ') -> None:
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


################ special forms

def cond_form(clauses: list[Expression], env: Environment) -> Any:
    for clause in clauses:
        match clause:
            case ['else', *body]:
                for exp in body:
                    result = evaluate(exp, env)
                return result
            case [test, *body] if evaluate(test, env):
                for exp in body:
                    result = evaluate(exp, env)
                return result


def or_form(expressions: list[Expression], env: Environment) -> Any:
    value = False
    for exp in expressions:
        value = evaluate(exp, env)
        if value:
            return value
    return value


def and_form(expressions: list[Expression], env: Environment) -> Any:
    value = True
    for exp in expressions:
        value = evaluate(exp, env)
        if not value:
            return value
    return value

################ eval

KEYWORDS = ['quote', 'if', 'define', 'set!', 'lambda', 'cond', 'or', 'and']


def evaluate(exp: Expression, env: Environment) -> Any:
    "Evaluate an expression in an environment."
    while True:
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
                    exp = consequence
                else:
                    exp = alternative
            case ['define', Symbol(var), value_exp]:            # (define var exp)
                env[var] = evaluate(value_exp, env)
                return
            case ['set!', Symbol(var), value_exp]:              # (set! var exp)
                env.change(var, evaluate(value_exp, env))
                return
            case ['define', [Symbol(name), *parms], *body       # (define (name parm...) body1 bodyN...)
                ] if len(body) > 0:
                env[name] = Procedure(parms, body, env)
                return
            case ['lambda', [*parms], *body] if len(body) > 0:  # (lambda (parm...) body1 bodyN...)
                return Procedure(parms, body, env)
            case ['cond', *clauses]:                            # (cond (t1 e1) (t2 e2)... (else eN))
                return cond_form(clauses, env)
            case ['or', *expressions]:                          # (or exp...)
                return or_form(expressions, env)
            case ['and', *expressions]:                         # (and exp...)
                return and_form(expressions, env)
            case [op, *args] if op not in KEYWORDS:             # (proc arg...)
                proc = evaluate(op, env)
                values = (evaluate(arg, env) for arg in args)
                if TCO_ENABLED and isinstance(proc, Procedure):
                     exp = ['begin', *proc.body]
                     local_env = dict(zip(proc.parms, values))
                     env = Environment(local_env, proc.env)
                else:
                    try:
                        return proc(*values)
                    except TypeError as exc:
                        msg = f'{exc!r} invoking {proc!r}({args!r}): {lispstr(exp)}\nAST={exp!r}'
                        raise EvaluatorException(msg) from exc
            case _:
                raise InvalidSyntax(lispstr(exp))


if __name__ == '__main__':
    repl()
