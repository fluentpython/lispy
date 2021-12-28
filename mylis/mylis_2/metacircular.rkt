#lang racket/base

(define (MAKE-GLOBAL-ENV) (list
    (list (quote NOT) not)
    (list (quote EQ?) eq?)
    (list (quote EQUAL?) equal?)
    (list (quote ADD) +)
    (list (quote SUB) -)
    (list (quote MUL) *)
    (list (quote DIV) /)
    (list (quote MAP) map)
    (list (quote LIST) list)
    (list (quote APPEND) append)
))

(define GLOBAL-ENV (MAKE-GLOBAL-ENV))

(define (LOOKUP name env)
    (cond
        ((null? env) #f)
        ((equal? name (car (car env))) (car (cdr (car env))))
        (else (LOOKUP name (cdr env)))))

(define (REST list) (cdr list))
(define (FIRST list) (car list))
(define (SECOND list) (car (cdr list)))
(define (THIRD list) (car (cdr (cdr list))))
(define (FOURTH list) (car (cdr (cdr (cdr list)))))

(define (EVLIS exps env)
    (if (null? exps)
        (quote ())
        (cons (EVAL (FIRST exps) env)
              (EVLIS (REST exps) env))
    )
)

(define (EVAL exp env)
    (cond
        ((number? exp) exp)
        ((symbol? exp) (LOOKUP exp env))
        ((equal? (FIRST exp) (quote QUOTE))
            (SECOND exp)        )
        ((equal? (FIRST exp) (quote IF))
            (if (EVAL (SECOND exp) env)
                (EVAL (THIRD exp) env)
                (EVAL (FOURTH exp) env)))
        ((equal? (FIRST exp) (quote LAMBDA))
            (MAKE-PROCEDURE (SECOND exp) (THIRD exp) env))
        ((equal? (FIRST exp) (quote DEFINE-GLOBAL))
            (set! GLOBAL-ENV (cons (list (SECOND exp) (EVAL (THIRD exp) env)) GLOBAL-ENV)))
        (else
            (apply (EVAL (FIRST exp) env) (EVLIS (REST exp) env)))))

(define (MAKE-PROCEDURE parms body definition-env)
    (lambda (args)
        (EVAL body
            (append
                (map list parms (list args))
                definition-env))
    )
)

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
(check-equal? (begin (EVAL '(DEFINE-GLOBAL X 8) GLOBAL-ENV) (FIRST GLOBAL-ENV)) '(X 8)) ; how do I inspect the GLOBAL-ENV?
; I don't yet know how to write DEFINE, because it would need to update the local environment
(check-equal? (begin (EVAL '(DEFINE-GLOBAL X2 (LAMBDA (N) (MUL N 2))) GLOBAL-ENV) (EVAL '(X2 8) GLOBAL-ENV)) 16)
                      
              