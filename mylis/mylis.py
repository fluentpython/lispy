#!/usr/bin/env python3

################ mylis: Scheme Environoment in Python 3.10
## Runtime support by Luciano Ramalho for lis.py
## (c) Peter Norvig, 2010-18; See http://norvig.com/lispy.html
################ imports and types

import sys
from collections.abc import Sequence, Iterator
from typing import Any, Protocol, Callable, NoReturn


import lis
from exceptions import UndefinedSymbol, UnexpectedCloseParen, EvaluatorException

################ non-interactive execution


def run_lines(source: str, env: lis.Environment | None = None) -> Iterator[Any]:
    global_env: lis.Environment = lis.standard_env()
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


############### multi-line lis.REPL


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
                   prompt2: str= '... ',
                   error_mark: str= '***',
                   *,
                   quit_cmd: str = QUIT_COMMAND,
                   input_fn: InputFn = input) -> None:
    """Read-Eval-Print-Loop"""

    global_env: lis.Environment = lis.standard_env()

    print(f'To exit, type {QUIT_COMMAND}', file=sys.stderr)

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
            result = str(lis.evaluate(current_exp, global_env))
        except EvaluatorException as exc:
            print(error_mark, exc)
            continue

        # ___________________________________________ Print
        print(result)


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
ERROR_MARK = '\N{WARNING SIGN} '

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
                print(f'{ERROR_MARK}  {key!r} was not defined.')
                cmd = ' '.join(args)
                print('    You can define it as an option:')
                print(f'    $ {cmd} {key}=<value>')


if __name__ == '__main__':
    main(sys.argv)
