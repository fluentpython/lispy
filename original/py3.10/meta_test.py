import operator as op

from lis import run

env_scm = """
(begin
    (define standard-env (list
        (list (quote +) +)
        (list (quote -) -)
    ))
    standard-env
)
"""

def test_env_build():
    got = run(env_scm)
    assert got == [['+', op.add], ['-', op.sub]]

scan_scm = """
(begin
    (define l (quote (a b c)))
    (define (scan what where)
        (if (null? where)
            ()
            (if (eq? what (car where))
                what
                (scan what (cdr where))))
    )
    {}
)
"""


def test_scan():
    source = scan_scm.format('(scan (quote a) l )')
    got = run(source)
    assert got == 'a'


def test_scan_not_found():
    source = scan_scm.format('(scan (quote z) l )')
    got = run(source)
    assert got is None


lookup_scm = """
(begin
    (define env (list
        (list (quote +) +)
        (list (quote -) -)
    ))
    (define (lookup what where)
        (if (null? where)
            ()
            (if (eq? what (car (car where)))
                (car (cdr (car where)))
                (lookup what (cdr where))))
    )
    {}
)
"""

def test_lookup():
    source = lookup_scm.format('(lookup (quote +) env)')
    got = run(source)
    assert got == op.add


def test_lookup_not_found():
    source = lookup_scm.format('(lookup (quote z) env )')
    got = run(source)
    assert got is None

