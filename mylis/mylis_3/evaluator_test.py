import ast
from typing import Any

from environ import core_env
from evaluator import evaluate, KEYWORDS, Environment
from parser import parse
from mytypes import Symbol

from pytest import fixture, mark

@fixture
def std_env() -> Environment:
    return core_env()


def test_evaluate_variable() -> None:
    env = Environment(dict(x = 10))
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


def test_evaluate_quote(std_env: Environment) -> None:
    source = '(quote (1.1 is not 1))'
    expected = [1.1, 'is', 'not', 1]
    got = evaluate(parse(source), std_env)
    assert got == expected


def test_evaluate_lambda(std_env: Environment) -> None:
    source = '(lambda (a b) (if (>= a b) a b))'
    func = evaluate(parse(source), std_env)
    assert func.parms == ['a', 'b']
    assert len(func.body) == 1
    assert func.body[0] == ['if', ['>=', 'a', 'b'], 'a', 'b']
    assert func.definition_env is std_env
    assert func(1, 2) == 2
    assert func(3, 2) == 3


def test_define_procedure(std_env: Environment) -> None:
    source = '(define (max a b) (if (>= a b) a b))'
    got = evaluate(parse(source), std_env)
    assert got is None
    max_fn = std_env['max']
    assert max_fn.parms == ['a', 'b']
    assert len(max_fn.body) == 1
    assert max_fn.body[0] == ['if', ['>=', 'a', 'b'], 'a', 'b']
    assert max_fn.definition_env is std_env
    assert max_fn(1, 2) == 2
    assert max_fn(3, 2) == 3


def test_call_user_procedure(std_env: Environment) -> None:
    source = """
        (begin
            (define max (lambda (a b) (if (>= a b) a b)))
            (max 22 11)
        )
        """
    got = evaluate(parse(source), std_env)
    assert got == 22

def test_evaluate_lambda_with_multi_expression_body(std_env: Environment) -> None:
    source = """
        (lambda (m n)
            (define (mod m n)
                (- m (* n (quotient m n))))
            (define (gcd m n)
                (if (= n 0)
                    m
                    (gcd n (mod m n))))
            (gcd m n)
        )
    """
    func = evaluate(parse(source), std_env)
    assert func.parms == ['m', 'n']
    assert len(func.body) == 3
    assert func(18, 45) == 9    


source = """
(begin
    (define (make-adder increment)
        (lambda (x) (+ increment x))
    )
    (define x 0)
    (define inc (make-adder 8))
    (inc 34)
)
"""
def test_closure(std_env: Environment) -> None:
    got = evaluate(parse(source), std_env)
    assert got == 42

# Consistency check

# TODO: consider moving test below as a self test in evaluator.py
def test_declared_keywords():
    """ Check that the set of KEYWORDS is the same as
        the set of string constants in the first position of
        sequence patterns in the match/case inside evaluate()
    """
    with open('evaluator.py') as source:
        tree = ast.parse(source.read())

    cases = None
    for node in ast.walk(tree):
        match node:
            # find FunctionDef named 'evaluate'
            case ast.FunctionDef(name='evaluate', body=[_,
                        ast.Match(cases=cases)
                    ]):
                break
    assert cases is not None

    # collect string constants in the first position of sequence patterns
    found_keywords = set()
    for case in cases:
        match case.pattern:
            case ast.MatchSequence(
                    patterns=[ast.MatchValue(ast.Constant(value=str(kw))), *_]):
                found_keywords.add(kw)

    assert sorted(found_keywords) == sorted(KEYWORDS)