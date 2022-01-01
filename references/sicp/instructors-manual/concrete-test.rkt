#lang racket/base

(require rackunit
         "concrete.rkt")

(check-false (ASSQ 'x '()) "Empty bindings")
(check-equal? (ASSQ 'x '([x 8])) '(x 8) "First binding")
(check-equal? (ASSQ 'x '([y 7] [x 8])) '(x 8) "Older binding")
(check-equal? (ASSQ 'z '([x 8])) #f "No binding")


(check-exn (regexp "Unbound variable 'x") (lambda ()
    (LOOKUP-VARIABLE-VALUE 'x '())) "Wrong error")
; WIP:
; (check-equal? (LOOKUP-VARIABLE-VALUE 'x '([x 8])) 8)
