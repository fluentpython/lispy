#!/usr/bin/env python3

################ mylis: Tiny Scheme Environment in Python 3.10
## Additional runtime support by Luciano Ramalho for lis.py by
## Peter Norvig (c) 2010-18; See http://norvig.com/lispy.html


import functools as ft
import itertools as it
import operator as op
import math
import readline  # "unused" import to enable readline in input()
import sys
from collections import ChainMap
from collections.abc import Sequence, Iterator
from typing import Any, Protocol, Callable, NoReturn

import lis
from exceptions import UndefinedSymbol, UnexpectedCloseParen, EvaluatorException


################ enhanced and new built-ins


def s_expr(obj: object) -> str:
    "Convert Python object into Lisp s-expression."
    match obj:
        case True:
            return '#t'
        case False:
            return '#f'
        case list(obj):
            items = ' '.join(s_expr(x) for x in obj)
            return f'({items})'
        case lis.Symbol(x):
            return x
        case _:
            return repr(obj)


def display(obj: object) -> str:
    output = s_expr(obj)
    print(output)


def variadic_sub(first, *rest):
    if rest:
        return first - sum(rest)
    else:
        return -first


def variadic_truediv(first, *rest):
    if rest:
        return first / math.prod(rest)
    else:
        return 1 / first


def variadic_comparison(op, current, *rest):
    for arg in rest:
        if not op(current, arg):
            return False
        current = arg
    return True


def standard_env() -> lis.Environment:
    env = lis.standard_env()
    env.update({
        # enhancements
        '#f': False,
        '#t': True,
        '+':  lambda *args: sum(args),
        '-':  variadic_sub,
        '*':  lambda *args: math.prod(args),
        '/':  variadic_truediv,
        '=':  lambda first, *rest: all(first == x for x in rest),
        '<':  ft.partial(variadic_comparison, op.lt),
        '>':  ft.partial(variadic_comparison, op.gt),
        '<=': ft.partial(variadic_comparison, op.le),
        '>=': ft.partial(variadic_comparison, op.ge),
        'append': lambda *args: list(it.chain(*args)),
        # additional built-ins
        'quotient': op.floordiv,
        'display': display,
        'filter': lambda *args: list(filter(*args)),
    })
    return env


################ non-interactive execution


def run_lines(source: str, env: lis.Environment | None = None) -> Iterator[Any]:
    global_env: lis.Environment = ChainMap({}, standard_env())
    if env is not None:
        global_env.update(env)
    tokens = lis.tokenize(source)
    while tokens:
        exp = lis.read_from_tokens(tokens)
        yield lis.evaluate(exp, global_env)


def run(source: str, env: lis.Environment | None = None) -> Any:
    for result in run_lines(source, env):
        pass
    return result


############### multi-line REPL


class QuitRequest(Exception):
    """Signal to quit multi-line input."""


ELLIPSIS = '\N{HORIZONTAL ELLIPSIS}'


def raise_unexpected_paren(line: str) -> NoReturn:
    max_msg_len = 16
    if len(line) < max_msg_len:
        msg = line
    else:
        msg = ELLIPSIS + line[-(max_msg_len-1):]
    raise UnexpectedCloseParen(msg)


QUIT_COMMAND = '.q'
InputFn = Callable[[str], str]

def multiline_input(prompt1: str,
                    prompt2: str,
                    *,
                    quit_cmd: str = QUIT_COMMAND,
                    input_fn: InputFn = input) -> str:

    paren_cnt = 0
    lines = []
    prompt = prompt1
    while True:
        line = input_fn(prompt).rstrip()
        if line == quit_cmd:
            raise QuitRequest()
        for char in line:
            if char == '(':
                paren_cnt += 1
            elif char == ')':
                paren_cnt -= 1
            if paren_cnt < 0:
                raise_unexpected_paren(line)
        lines.append(line)
        prompt = prompt2
        if paren_cnt == 0:
            break

    return '\n'.join(lines)


def multiline_repl(prompt1: str = '> ',
                   prompt2: str = '... ',
                   error_mark: str = '***',
                   *,
                   quit_cmd: str = QUIT_COMMAND,
                   input_fn: InputFn = input) -> None:
    """Read-Eval-Print-Loop"""

    global_env: lis.Environment = ChainMap({}, standard_env())

    print(f'To exit type {QUIT_COMMAND}', file=sys.stderr)

    while True:
        # ___________________________________________ Read
        try:
            source = multiline_input(prompt1, prompt2,
                                     quit_cmd=quit_cmd,
                                     input_fn=input_fn)
        except (EOFError, QuitRequest):
            break
        except UnexpectedCloseParen as exc:
            print(error_mark, exc)
            continue
        if not source:
            continue

        # ___________________________________________ Eval
        current_exp = lis.parse(source)
        try:
            result = lis.evaluate(current_exp, global_env)
        except EvaluatorException as exc:
            print(error_mark, exc)
            continue

        # ___________________________________________ Print
        if result is not None:
            print(s_expr(result))


############### command-line integration

class TextReader(Protocol):
    def read(self) -> str:
        ...


def run_file(source_file: TextReader, env: lis.Environment | None = None) -> Any:
    source = source_file.read()
    return run(source, env)


def env_from_args(args: Sequence[str]) -> lis.Environment:
    env = {}
    for arg in (a for a in args if '=' in a):
        parts = arg.split('=')
        if len(parts) != 2 or not all(parts):
            continue
        name, val = parts
        try:
            atom = lis.parse_atom(val)
        except ValueError:
            continue
        env[name] = atom
    return env

############### main

PROMPT1 = '\N{WHITE RIGHT-POINTING TRIANGLE}  '
PROMPT2 = '\N{MIDLINE HORIZONTAL ELLIPSIS}    '
ERROR_MARK = '\N{POLICE CARS REVOLVING LIGHT} '

def main(args: list[str]) -> None:
    if len(args) == 1:
        multiline_repl(PROMPT1, PROMPT2, ERROR_MARK)
    else:
        arg_env = env_from_args(args[1:])
        with open(args[1]) as source_file:
            try:
                run_file(source_file, arg_env)
            except UndefinedSymbol as exc:
                key = exc.args[0]
                print(f'{ERROR_MARK} {key!r} was not defined.')
                cmd = ' '.join(args)
                print('    You can define it as an option:')
                print(f'    $ {cmd} {key}=<value>')


if __name__ == '__main__':
    main(sys.argv)
