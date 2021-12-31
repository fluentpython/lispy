#lang racket/base

(require "metacircular.rkt")
(require rackunit)


; LOOKUP
(check-equal? (LOOKUP 'ADD GLOBAL-ENV) + "Existing name")
(check-equal? (LOOKUP 'NO-SUCH-NAME GLOBAL-ENV) #f "Non-existing name")

; EVAL
(check-equal? (EVAL '1 '()) 1 "Number literal")
(check-equal? (EVAL 'ADD GLOBAL-ENV) + "Built-in procedure")
(check-equal? (EVAL '(QUOTE (A (B C))) GLOBAL-ENV) '(A (B C)) "QUOTE form")
(check-equal? (EVAL '(IF (EQUAL? 3 3) 1 0) GLOBAL-ENV) 1 "IF form, consequence branch")
(check-equal? (EVAL '(IF (EQUAL? 3 4) 1 0) GLOBAL-ENV) 0 "IF form, alternative branch")
(check-true (procedure? (EVAL '(LAMBDA (N) (MUL N 2)) GLOBAL-ENV)) "LAMBDA form, definition")
(check-equal? (EVAL '((LAMBDA (N) (MUL N 2)) 4) GLOBAL-ENV) 8 "LAMBDA form, application")
(check-equal? (begin (EVAL '(DEFINE-GLOBAL X 8) GLOBAL-ENV) (car GLOBAL-ENV)) '(X 8)) ; how do I inspect the GLOBAL-ENV?
; I don't yet know how to write DEFINE, because it would need to update the local environment
(check-equal? (begin (EVAL '(DEFINE-GLOBAL X2 (LAMBDA (N) (MUL N 2))) GLOBAL-ENV) (EVAL '(X2 8) GLOBAL-ENV)) 16)
