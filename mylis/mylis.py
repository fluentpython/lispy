#!/usr/bin/env python3

################ mylis: Scheme Environoment in Python 3.10
## Runtime support by Luciano Ramalho for lis.py
## (c) Peter Norvig, 2010-18; See http://norvig.com/lispy.html
################ imports and types

import sys
from collections.abc import Sequence
from typing import Any, Protocol, Callable, NoReturn


import lis
from exceptions import UnexpectedCloseParen, EvaluatorException

############### multi-line lis.REPL


class QuitRequest(Exception):
    """Signal to quit multi-line input."""


QUIT_COMMAND = '.q'
ELLIPSIS = '\N{HORIZONTAL ELLIPSIS}'


def raise_unexpected_paren(line: str) -> NoReturn:
    max_msg_len = 16
    if len(line) < max_msg_len:
        msg = line
    else:
        msg = ELLIPSIS + line[-(max_msg_len-1):]
    raise UnexpectedCloseParen(msg)


InputFnType = Callable[[str], str]

def multiline_input(prompt1: str = '-->',
                    prompt2: str= f' {ELLIPSIS}>',
                    *,
                    quit_cmd: str = QUIT_COMMAND,
                    input_fn: InputFnType = input) -> str:

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


def repl(input_fn: InputFnType = input) -> None:
    """Read-Eval-Print-Loop"""

    global_env: lis.Environment = lis.standard_env()

    print(f'To exit, type {QUIT_COMMAND}', file=sys.stderr)

    while True:
        # ___________________________________________ Read
        try:
            source = multiline_input('> ', '... ',
                                     quit_cmd=QUIT_COMMAND,
                                     input_fn=input_fn)
        except (EOFError, QuitRequest):
            break
        except UnexpectedCloseParen as exc:
            print('***', exc)
            continue
        if not source:
            continue

        # ___________________________________________ Eval
        current_exp = lis.parse(source)
        try:
            result = str(lis.evaluate(current_exp, global_env))
        except EvaluatorException as exc:
            print('***', exc)
            continue

        # ___________________________________________ Print
        print(result)


############### command-line integration

class TextReader(Protocol):
    def read(self) -> str:
        ...


def run_file(source_file: TextReader, env: lis.Environment | None = None) -> Any:
    source = source_file.read()
    return lis.run(source, env)


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


ERROR_MARK = '\N{POLICE CARS REVOLVING LIGHT}'


def main(args: list[str]) -> None:
    if len(args) == 1:
        lis.repl()
    else:
        arg_env = env_from_args(args[1:])
        with open(args[1]) as source_file:
            try:
                run_file(source_file, arg_env)
            except KeyError as exc:
                key = exc.args[0]
                print(f'{ERROR_MARK}  {key!r} was not defined.')
                cmd = ' '.join(args)
                print('    You can define it as an option:')
                print(f'    $ {cmd} {key}=<value>')


if __name__ == '__main__':
    main(sys.argv)
