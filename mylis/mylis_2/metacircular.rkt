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

(provide LOOKUP GLOBAL-ENV EVAL)
              