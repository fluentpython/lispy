from .listypes import (
    Expression,
    ParserException,
    UnexpectedCloseBrace,
    UnexpectedEndOfSource,
)
from .parser import parse

from pytest import mark, raises


@mark.parametrize(
    'source, expected',
    [
        ('7', 7),
        ('x', 'x'),
        ('(sum 1 2 3)', ['sum', 1, 2, 3]),
        ('(+ (* 2 100) (* 1 10))', ['+', ['*', 2, 100], ['*', 1, 10]]),
        ('99 100', 99),  # parse stops at the first complete expression
        ('(a)(b)', ['a']),
    ],
)
def test_parse(source: str, expected: Expression) -> None:
    got = parse(source)
    assert got == expected


@mark.parametrize(
    'source, expected',
    [
        ('(if (< x 0) 0 x)', ['if', ['<', 'x', 0], 0, 'x']),
        ('{if (< x 0) 0 x}', ['if', ['<', 'x', 0], 0, 'x']),
        (
            """(cond
                ((> x 0) x)
                ((= x 0) 0)
                ((< x 0) (- 0 x)))
         """,
            [
                'cond',
                [['>', 'x', 0], 'x'],
                [['=', 'x', 0], 0],
                [['<', 'x', 0], ['-', 0, 'x']],
            ],
        ),
        (
            """{cond
                [(> x 0) x]
                [(= x 0) 0]
                [(< x 0) (- 0 x)]}
         """,
            [
                'cond',
                [['>', 'x', 0], 'x'],
                [['=', 'x', 0], 0],
                [['<', 'x', 0], ['-', 0, 'x']],
            ],
        ),
    ],
)
def test_parse_mixed_braces(source: str, expected: Expression) -> None:
    got = parse(source)
    assert got == expected


@mark.parametrize(
    'source, expected',
    [
        ('', UnexpectedEndOfSource),
        ('(', UnexpectedEndOfSource),
        ('([]', UnexpectedEndOfSource),
        ('(])', UnexpectedCloseBrace),
        ('([)', UnexpectedCloseBrace),
    ],
)
def test_parse_malformed(source: str, expected: ParserException) -> None:
    with raises(expected):  # type: ignore
        parse(source)
