import collections
from typing import Any, cast

from parser import s_expr
from mytypes import Expression, InvalidSyntax, Symbol, UndefinedSymbol
from environ import Environment, core_env

class Procedure:
    "A user-defined Scheme procedure."

    def __init__(
        self, parms: list[Symbol], body: list[Expression], env: Environment
    ):
        self.parms = parms
        self.body = body
        self.definition_env = env

    def __call__(self, *args: Expression) -> Any:
        local_env = dict(zip(self.parms, args))
        env = Environment(local_env, self.definition_env)
        for exp in self.body:
            result = evaluate(exp, env)
        return result


KEYWORDS = 'define if quote lambda'.split()

# Quantifiers in s-expression syntax comments:
#   * : 0 or more
#   + : 1 or more
#   ? : 0 or 1

def evaluate(exp: Expression, env: Environment) -> Any:
    "Evaluate an expression in an environment."
    match exp:
        case int(x) | float(x):                        # number literal
            return x
        case Symbol(var):                              # variable reference
            try:
                return env[var]
            except KeyError as exc:
                raise UndefinedSymbol(var) from exc
        case ['define', Symbol(var), value_exp]:       # (define var exp)
            env[var] = evaluate(value_exp, env)
        case ['define',                                # (define (name parm*)) body+)
                    [Symbol(name), *parms], *body  
                ] if len(body) > 0:
            env[name] = Procedure(parms, body, env)  # type: ignore[has-type]
        case ['if', test, consequence, alternative]:   # (if test consequence alternative)
            if evaluate(test, env):
                return evaluate(consequence, env)
            else:
                return evaluate(alternative, env)
        case ['quote', exp]:                           # (quote exp)
            return exp
        case ['lambda', [*parms], *body] if len(body) > 0:  # (lambda (parm*) body+)
            return Procedure(parms, body, env)  # type: ignore[has-type]
        case [op, *args] if op not in KEYWORDS:        # (op exp*)
            proc = evaluate(op, env)
            values = [evaluate(arg, env) for arg in args]
            return proc(*values)
        case _:
            raise InvalidSyntax(s_expr(exp))
