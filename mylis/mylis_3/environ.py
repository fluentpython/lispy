import math
import operator as op

from .mytypes import Environment, Symbol


def core_env() -> Environment:
    "An environment for an s-expression calculator."
    # sin, cos, sqrt, pi, ...
    env: Environment = {Symbol(name): obj for name, obj in vars(math).items()}
    env.update(
        {
            Symbol('+'): op.add,
            Symbol('-'): op.sub,
            Symbol('*'): op.mul,
            Symbol('/'): op.truediv,
            Symbol('quotient'): op.floordiv,
            Symbol('>'): op.gt,
            Symbol('<'): op.lt,
            Symbol('>='): op.ge,
            Symbol('<='): op.le,
            Symbol('='): op.eq,
            Symbol('abs'): abs,
            Symbol('begin'): lambda *x: x[-1],
            Symbol('eq?'): op.is_,
            Symbol('equal?'): op.eq,
            Symbol('max'): max,
            Symbol('min'): min,
            Symbol('not'): op.not_,
            Symbol('number?'): lambda x: isinstance(x, (int, float)),
            Symbol('procedure?'): callable,
            Symbol('modulo'): op.mod,
            Symbol('round'): round,
            Symbol('symbol?'): lambda x: isinstance(x, Symbol),
        }
    )
    return env