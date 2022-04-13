#!/usr/bin/env python3

################ Scheme Interpreter in Python 3.10
## Based on lis.py (c) Peter Norvig, 2010-18
## See http://norvig.com/lispy.html
## Refactorting and additions by Luciano Ramalho (2022)

from .lis_types import (
    Atom,
    Symbol,
    Expression,
    BlankExpression,
    BraceNeverClosed,
    UnexpectedCloseBrace,
)


def parse(source: str) -> Expression:
    """Read a Scheme expression from a string."""
    return read_from_tokens(tokenize(source))


BRACES = {
    '(': ')',
    '[': ']',
    '{': '}',
}
CLOSE_BRACES = BRACES.values()


def tokenize(s: str) -> list[str]:
    """Convert a string into a list of tokens."""
    for open, close in BRACES.items():
        s = s.replace(open, f' {open} ').replace(close, f' {close} ')
    return s.split()


def read_from_tokens(tokens: list[str]) -> Expression:
    """Read an expression from a sequence of tokens."""
    try:
        token = tokens.pop(0)
    except IndexError:
        raise BlankExpression()
    if token in BRACES:
        exp = []
        while tokens and tokens[0] != BRACES[token]:
            exp.append(read_from_tokens(tokens))
        if not tokens:
            raise BraceNeverClosed(token)
        tokens.pop(0)  # discard close brace
        return exp
    elif token in CLOSE_BRACES:
        raise UnexpectedCloseBrace(token)
    else:
        return parse_atom(token)


def parse_atom(token: str) -> Atom:
    """Numbers become numbers; every other token is a symbol."""
    try:
        return int(token)
    except ValueError:
        try:
            return float(token)
        except ValueError:
            return Symbol(token)


def lispstr(exp: object) -> str:
    """Convert a Python object back into a Lisp-readable string."""
    if isinstance(exp, list):
        return '(' + ' '.join(map(lispstr, exp)) + ')'
    else:
        return str(exp)
