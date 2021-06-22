import math

from lis import run


fact_scm = """
(define (! n)
    (if (< n 2)
        1
        (* n (! (- n 1)))
    )
)
(! 42)
"""
def test_factorial():
    got = run(fact_scm)
    assert got == 1405006117752879898543142606244511569936384000000000
    assert got == math.factorial(42)
    

gcd_scm = """
(define (mod m n)
    (- m (* n (// m n))))
(define (gcd m n)
    (if (= n 0)
        m
        (gcd n (mod m n))))
(gcd 18 45)
"""
def test_gcd():
    got = run(gcd_scm)
    assert got == 9


quicksort_scm = """
(define (quicksort lst)
    (if (null? lst)
        lst
        (begin
            (define pivot (car lst))
            (define rest (cdr lst))
            (append
                (quicksort
                    (filter (lambda (x) (< x pivot)) rest))
                (list pivot)
                (quicksort
                    (filter (lambda (x) (>= x pivot)) rest)))
        )
    )
)
(quicksort (list 2 1 6 3 4 0 8 9 7 5))
"""
def test_quicksort():
    got = run(quicksort_scm)
    assert got == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]


# Example from Structure and Interpretation of Computer Programs
# https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node12.html

newton_scm = """
(define (sqrt x)
    (sqrt-iter 1.0 x))
(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
(define (good-enough? guess x)
    (< (abs (- (* guess guess) x)) 0.001))
(define (improve guess x)
    (average guess (/ x guess)))
(define (average x y)
    (/ (+ x y) 2))
(sqrt 123454321)
"""
def test_newton():
    got = run(newton_scm)
    assert math.isclose(got, 11111)
