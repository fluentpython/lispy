from collections.abc import MutableMapping
from typing import TypeAlias


# cannot use NewType because we need isinstance()
# to support (symbol? x) in environ.py
Symbol: TypeAlias = str
Number: TypeAlias = int | float
Atom: TypeAlias = int | float | Symbol
Expression: TypeAlias = Atom | list  # type: ignore


class InterpreterException(Exception):
    """Generic interpreter exception."""

    def __init__(self, value: str = ''):
        self.value = value

    def __str__(self) -> str:
        msg = self.__class__.__doc__ or ''
        if self.value:
            msg = msg.rstrip('.')
            if "'" in self.value:
                value = self.value
            else:
                value = repr(self.value)
            msg += f': {value}'
        return msg


class ParserException(InterpreterException):
    """Generic exception while parsing."""


class UnexpectedCloseBrace(ParserException):
    """Unexpected close brace."""


class BraceNeverClosed(ParserException):
    """Open brace was never closed."""


class EvaluatorException(InterpreterException):
    """Exception while evaluating."""


class InvalidSyntax(EvaluatorException):
    """Invalid syntax."""


class UndefinedSymbol(EvaluatorException):
    """Undefined symbol."""
