#lang racket/base

(require rackunit
         "concrete2.rkt")

(check-false (ASSQ 'x '()) "Empty bindings")
(check-equal? (ASSQ 'x '([x . 8])) '(x . 8) "First binding")
(check-equal? (ASSQ 'x '([y . 7] [x . 8])) '(x . 8) "Older binding")
(check-equal? (ASSQ 'z '([x . 8])) #f "No binding")

(check-equal? (MAKE-FRAME '(a) '(1)) '([a . 1]) "Smalest frame")
(check-equal? (MAKE-FRAME '(x y z) '(21 22 23)) '((x . 21) (y . 22) (z . 23)) "3 bindings")

(check-equal? (EXTEND-ENVIRONMENT '(a b) '(1 2) '()) '(([a . 1] [b . 2])))
(check-equal? (EXTEND-ENVIRONMENT '() '() '()) '(()) )

(check-equal? (EXTEND-ENVIRONMENT '(a b) '(1 2)
                                  (EXTEND-ENVIRONMENT '(c d) '(3 4) '()))
              '(((a . 1) (b . 2))
                ((c . 3) (d . 4))) "Two frames, each with two bindings.")

(check-exn (regexp "Unbound variable 'x") (lambda ()
    (LOOKUP-VARIABLE-VALUE 'x '())))

(check-equal? (LOOKUP-VARIABLE-VALUE 'x '(([x . 8]))) 8)

(check-equal? (EVAL 1 '()) 1)
(check-equal? (EVAL '(QUOTE symbol) '()) 'symbol)
(check-equal? (EVAL '(QUOTE (1 2 3)) '()) '(1 2 3))
(check-equal? (EVAL 'x (EXTEND-ENVIRONMENT '(x) '(8) '())) 8)
(check-equal? (EVAL '(+ 3 4) (MAKE-BUILTINS)) 7)

; WIP: replace MAKE-BUILTINS with MAKE-BUILTIN-ENVIRONMENT
(check-equal? (car (MAKE-BUILTIN-ENVIRONMENT)) '(+ . +))
(check-equal? (caar (MAKE-BUILTIN-ENVIRONMENT)) '+)
;(check-equal? (EVAL '(+ 3 4) (MAKE-BUILTIN-ENVIRONMENT)) 7)

(check-equal? (EVAL '(+ (* 3 4) (* 5 6)) (MAKE-BUILTINS)) 42)
(check-equal? (EVAL '((LAMBDA (x) (* x x)) 111) (MAKE-BUILTINS)) 12321)
(check-equal? (EVAL '(((LAMBDA (x)
                               (LAMBDA (y) (+ x y)))
                       3)
                      4)
                    (MAKE-BUILTINS)) 7)

(check-equal? (EVAL '((LAMBDA (x y)
                               ((LAMBDA (y) (+ x y))
                                (* x y)))
                       3 4)
                    (MAKE-BUILTINS)) 15)

(check-equal? (EVAL '((LAMBDA (x) (IF (< x 0) (- 0 x) x)) 2)
                    (MAKE-BUILTINS)) 2)

(check-equal? (EVAL '((LAMBDA (x) (IF (< x 0) (- 0 x) x)) -3)
                    (MAKE-BUILTINS)) 3)
