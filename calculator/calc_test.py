from typing import Optional

from pytest import mark

from calc import parse, evaluate, Expression, global_env

# Norvig's tests are not isolated: they assume the
# same environment from first to last test.

@mark.parametrize( 'source, expected', [
    ('7', 7),
    ('x', 'x'),
    ('(sum 1 2 3)', ['sum', 1, 2, 3]),
    ('(+ (* 2 100) (* 1 10))', ['+', ['*', 2, 100], ['*', 1, 10]]),
])
def test_parse(source: str, expected: Expression) -> None:
    got = parse(source)
    assert got == expected


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
    got = evaluate(parse(source))
    assert got == expected

# tests for each of the cases in evaluate

def test_evaluate_variable() -> None:
    global_env['x'] = 10
    source = 'x'
    expected = 10
    got = evaluate(parse(source))
    assert got == expected


def test_evaluate_literal() -> None:
    source = '3.3'
    expected = 3.3
    got = evaluate(parse(source))
    assert got == expected


def test_evaluate_if_true() -> None:
    source = '(if 1 10 no-such-thing)'
    expected = 10
    got = evaluate(parse(source))
    assert got == expected


def test_evaluate_if_false() -> None:
    source = '(if 0 no-such-thing 20)'
    expected = 20
    got = evaluate(parse(source))
    assert got == expected


def test_define() -> None:
    source = '(define answer (* 6 7))'
    got = evaluate(parse(source))
    assert got is None
    assert global_env['answer'] == 42


def test_invocation_builtin_cos() -> None:
    source = '(cos 0)'
    got = evaluate(parse(source))
    assert got == 1.0


def test_invocation_builtin_random() -> None:
    source = '(random)'
    got = evaluate(parse(source))
    assert 0 <= got < 1
