from environ import core_env
from evaluator import evaluate
from mytypes import Expression
from parser import parse

from pytest import mark

@mark.parametrize('source, expected', [
    ('(+ 1 2)', 3),
    ('(+ 1)', 1),
    ('(+ 1 2 3)', 6),
    ('(+)', 0),
])
def test_variadic_add(source: str, expected: Expression) -> None:
    got = evaluate(parse(source), core_env())
    assert got == expected


@mark.parametrize('source, expected', [
    ('(= 3 3)', True),
    ('(= 1 3)', False),
    ('(= 2 2 2)', True),
    ('(= 2 2 1)', False),
])
def test_variadic_eq(source: str, expected: Expression) -> None:
    got = evaluate(parse(source), core_env())
    assert got == expected


@mark.parametrize('source, expected', [
    ('(= 3 3)', True),
    ('(= 1 3)', False),
    ('(= 2 2 2)', True),
    ('(= 2 2 1)', False),
    ('(< 3 3)', False),
    ('(< 1 3)', True),
    ('(< 1 2 2)', False),
    ('(< 1 2 3)', True),
    ('(<= 3 3)', True),
    ('(<= 1 3)', True),
    ('(<= 1 2 2)', True),
    ('(<= 1 2 3)', True),
    ('(<= 1 2 1)', False),
    ('(> 3 3)', False),
    ('(> 3 1)', True),
    ('(> 2 1 2)', False),
    ('(> 3 2 1)', True),
    ('(>= 3 1)', True),
    ('(>= 1 3)', False),
    ('(>= 3 2 1)', True),
    ('(>= 3 2 2)', True),
    ('(>= 2 1 2)', False),
])
def test_variadic_comparison(source: str, expected: Expression) -> None:
    got = evaluate(parse(source), core_env())
    assert got == expected