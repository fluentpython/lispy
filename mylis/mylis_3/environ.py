import math
import operator as op

from .mytypes import Environment, Symbol


def core_env() -> Environment:
    "An environment for an s-expression calculator."
    env: Environment = vars(math)  # sin, cos, sqrt, pi, ...
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
            'begin': lambda *x: x[-1],
            'eq?': op.is_,
            'equal?': op.eq,
            'max': max,
            'min': min,
            'not': op.not_,
            'number?': lambda x: isinstance(x, (int, float)),
            'procedure?': callable,
            'modulo': op.mod,
            'round': round,
            'symbol?': lambda x: isinstance(x, Symbol),
        }
    )
    return env