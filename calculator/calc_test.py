from typing import Optional

from pytest import mark, fixture

from calc import parse, evaluate, Expression, Environment, standard_env

# Norvig's tests are not isolated: they assume the
# same environment from first to last test.
global_env_for_first_test = standard_env()

@mark.parametrize( 'source, expected', [
    ("(+ 2 2)", 4),
    ("(+ (* 2 100) (* 1 10))", 210),
    ("(if (> 6 5) (+ 1 1) (+ 2 2))", 2),
    ("(if (< 6 5) (+ 1 1) (+ 2 2))", 4),
    ("(define x 3)", None),
    ("x", 3),
    ("(+ x x)", 6),
])
def test_evaluate(source: str, expected: Optional[Expression]) -> None:
    got = evaluate(parse(source), global_env_for_first_test)
    assert got == expected


@fixture
def std_env() -> Environment:
    return standard_env()

# tests for each of the cases in evaluate

def test_evaluate_variable() -> None:
    env: Environment = dict(x=10)
    source = 'x'
    expected = 10
    got = evaluate(parse(source), env)
    assert got == expected


def test_evaluate_literal(std_env: Environment) -> None:
    source = '3.3'
    expected = 3.3
    got = evaluate(parse(source), std_env)
    assert got == expected


def test_evaluate_if_true(std_env: Environment) -> None:
    source = '(if 1 10 no-such-thing)'
    expected = 10
    got = evaluate(parse(source), std_env)
    assert got == expected


def test_evaluate_if_false(std_env: Environment) -> None:
    source = '(if 0 no-such-thing 20)'
    expected = 20
    got = evaluate(parse(source), std_env)
    assert got == expected


def test_define(std_env: Environment) -> None:
    source = '(define answer (* 6 7))'
    got = evaluate(parse(source), std_env)
    assert got is None
    assert std_env['answer'] == 42


def test_invocation_builtin_cos(std_env: Environment) -> None:
    source = '(cos 0)'
    got = evaluate(parse(source), std_env)
    assert got == 1.0
