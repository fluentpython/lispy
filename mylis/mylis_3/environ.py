import collections
import functools as ft
import math
import operator as op

from mytypes import Symbol
from parser import s_expr

class Environment(collections.ChainMap):
    "A ChainMap that allows updating an item in-place."

    def change(self, key: Symbol, value: object) -> None:
        "Find where key is defined and change the value there."
        for map in self.maps:
            if key in map:
                map[key] = value
                return
        raise KeyError(key)


def core_env() -> Environment:
    "An environment for an s-expression calculator."
    env = Environment(vars(math))  # sin, cos, sqrt, pi, ...
    env.update(
        {
            '+': lambda *args: sum(args),
            '-': op.sub,
            '*': op.mul,
            '/': op.truediv,
            'quotient': op.floordiv,
            '=':  ft.partial(variadic_comparison, op.eq),
            '<':  ft.partial(variadic_comparison, op.lt),
            '>':  ft.partial(variadic_comparison, op.gt),
            '<=': ft.partial(variadic_comparison, op.le),
            '>=': ft.partial(variadic_comparison, op.ge),
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
            'display': display,
        }
    )
    return env


def display(obj: object) -> None:
    output = s_expr(obj)
    print(output)
    
    
def variadic_comparison(op, current, *rest):
    for arg in rest:
        if not op(current, arg):
            return False
        current = arg
    return True
