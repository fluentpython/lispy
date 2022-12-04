from typing import Any

from .environ import core_env
from .evaluator import evaluate
from .parser import parse
from .mytypes import Environment, Symbol

from pytest import fixture, mark

@fixture
def std_env() -> Environment:
    return core_env()


def test_evaluate_variable() -> None:
    env: Environment = {Symbol('x'): 10}
    source = 'x'
    expected = 10
    got = evaluate(parse(source), env)
    assert got == expected


def test_evaluate_literal(std_env: Environment) -> None:
    source = '3.3'
    expected = 3.3
    got = evaluate(parse(source), std_env)
    assert got == expected


@mark.parametrize( 'source, expected', [
    ('(* 11111 11111)', 123454321),
    ('(* (+ 4 3) 6)', 42),
    ('(sin (/ pi 2))', 1)
])
def test_evaluate_call(
    std_env: Environment,
    source: str,
    expected: Any,
) -> None:
    got = evaluate(parse(source), std_env)
    assert got == expected

# Special forms

def test_define_variable(std_env: Environment) -> None:
    source = '(define answer (* 6 7))'
    got = evaluate(parse(source), std_env)
    assert got is None
    assert std_env[Symbol('answer')] == 42
