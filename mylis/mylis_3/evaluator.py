from typing import Any

from .parser import s_expr
from .mytypes import Environment, Expression, InvalidSyntax, Symbol, UndefinedSymbol

KEYWORDS = 'define'.split()

# Quantifiers in s-expression syntax comments:
#   * : 0 or more
#   + : 1 or more
#   ? : 0 or 1

def evaluate(exp: Expression, env: Environment) -> Any:
    "Evaluate an expression in an environment."
    match exp:
        case int(x) | float(x):                      # number literal
            return x
        case Symbol(var):                            # variable reference
            try:
                return env[var]
            except KeyError as exc:
                raise UndefinedSymbol(var) from exc
        case ['define', Symbol(var), value_exp]:     # (define var exp)
            env[var] = evaluate(value_exp, env)
        case [op, *args] if op not in KEYWORDS:      # (op exp*)
            proc = evaluate(op, env)
            values = [evaluate(arg, env) for arg in args]
            return proc(*values)
        case _:
            raise InvalidSyntax(s_expr(exp))
