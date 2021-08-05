#!/usr/bin/env python3

################ mylis: Scheme Environoment in Python 3.10
## Runtime support by Luciano Ramalho for lis.py
## (c) Peter Norvig, 2010-18; See http://norvig.com/lispy.html
################ imports and types

import sys
from collections.abc import Sequence
from typing import Any, Protocol

from lis import Environment, parse_atom, run, repl

############### command-line integration


class TextReader(Protocol):
    def read(self) -> str:
        ...


def run_file(source_file: TextReader, env: Environment | None = None) -> Any:
    source = source_file.read()
    return run(source, env)


def env_from_args(args: Sequence[str]) -> Environment:
    env = {}
    for arg in (a for a in args if '=' in a):
        parts = arg.split('=')
        if len(parts) != 2 or not all(parts):
            continue
        name, val = parts
        try:
            atom = parse_atom(val)
        except ValueError:
            continue
        env[name] = atom
    return env


ERROR_MARK = '\N{POLICE CARS REVOLVING LIGHT}'


def main(args: list[str]) -> None:
    if len(args) == 1:
        repl()
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
