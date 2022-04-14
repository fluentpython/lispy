from email.parser import Parser
from .mytypes import (
    Expression,
    ParserException,
    UnexpectedCloseBrace,
    BraceNeverClosed,
)
from .parser import parse, s_expr

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
            """ (cond
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
            """ {cond
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
    'source, expected, match',
    [
        ('', ParserException, 'Empty'),
        ('{', BraceNeverClosed, '{'),
        ('([]', BraceNeverClosed, '('),
        ('(])', UnexpectedCloseBrace, ']'),
        ('([)', UnexpectedCloseBrace, ')'),
    ],
)
def test_parse_malformed(
    source: str, expected: ParserException, match: str
) -> None:
    with raises(expected) as excinfo:  # type: ignore
        parse(source)
    assert match in str(excinfo.value)


@mark.parametrize('obj, expected', [
    (0, '0'),
    (1, '1'),
    (False, '#f'),
    (True, '#t'),
    (1.5, '1.5'),
    ('sin', 'sin'),
    (['+', 1, 2], '(+ 1 2)'),
    (['if', ['<', 'a', 'b'], True, False], '(if (< a b) #t #f)'),
    ([], '()'),
    (None, 'None'),
    (..., 'Ellipsis'),
])
def test_s_expr(obj: object, expected: str) -> None:
    got = s_expr(obj)
    assert got == expected