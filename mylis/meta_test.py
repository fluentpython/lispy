import operator as op

import mylis

env_scm = """
(define standard-env (list
    (list (quote -) -)
    (list (quote /) /)
))
standard-env
"""

def test_env_build():
    got = mylis.run(env_scm)
    assert got == [['-', op.sub], ['/', op.truediv]]

scan_scm = """
(define l (quote (a b c)))
(define (scan what where)
    (if (null? where)
        ()
        (if (eq? what (car where))
            what
            (scan what (cdr where))))
)
"""

def test_scan():
    source = scan_scm + '(scan (quote a) l )'
    got = mylis.run(source)
    assert got == 'a'


def test_scan_not_found():
    source = scan_scm + '(scan (quote z) l )'
    got = mylis.run(source)
    assert got == []


lookup_scm = """
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
"""

def test_lookup():
    source = lookup_scm + '(lookup (quote -) env)'
    got = mylis.run(source)
    assert got == op.sub


def test_lookup_not_found():
    source = lookup_scm + '(lookup (quote z) env )'
    got = mylis.run(source)
    assert got == []

