#!/usr/bin/env python3

################ lis.py: Scheme Interpreter in Python 3.10
## (c) Peter Norvig, 2010-18; See http://norvig.com/lispy.html
## Refactorting and additions by Luciano Ramalho (2022)

from .listypes import (
    Atom,
    Symbol,
    Expression,
    UnexpectedCloseBrace,
    UnexpectedEndOfSource,
)


def parse(source: str) -> Expression:
    """Read a Scheme expression from a string."""
    return read_from_tokens(tokenize(source), [])


OPEN_BRACES = {
    '(': ')',
    '[': ']',
    '{': '}',
}
CLOSE_BRACES = OPEN_BRACES.values()


def tokenize(s: str) -> list[str]:
    """Convert a string into a list of tokens."""
    for open, close in OPEN_BRACES.items():
        s = s.replace(open, f' {open} ').replace(close, f' {close} ')
    return s.split()


def read_from_tokens(tokens: list[str], open_braces: list[str]) -> Expression:
    """Read an expression from a sequence of tokens."""
    try:
        token = tokens.pop(0)
    except IndexError:
        raise UnexpectedEndOfSource()
    if token in OPEN_BRACES:
        open_braces.append(token)
        exp = []
        while tokens and tokens[0] != OPEN_BRACES[token]:
            exp.append(read_from_tokens(tokens, open_braces))
        if not tokens:
            raise UnexpectedEndOfSource()
        tokens.pop(0)  # discard close brace
        return exp
    elif token in CLOSE_BRACES:
        raise UnexpectedCloseBrace()
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
