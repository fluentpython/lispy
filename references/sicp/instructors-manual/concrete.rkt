#lang racket/base

; code adapted from the "evaluator with concrete syntax"
; listed on pages 121-122 of the "Instructor's Manual to
; accompany Structure and Interpretation of Computer Programs"
; by Julie Sussman with Harold Abelson and Gerald Jay Sussman.

; Tested with Racket 8.3.
; Function names changed to ALL-CAPS to avoid potential
; confusion with functions provided by Racket.

(define (ASSQ var bindings)
  (cond [(null? bindings) #f]
        [(eq? var (caar bindings)) (car bindings)]
        [else (ASSQ var (cdr bindings))]))

(define (LOOKUP-VARIABLE-VALUE var env)
  (if (null? env)
      (error "Unbound variable" var)
      (let ([binding (ASSQ var (car env))])
        (if (eq? binding #f)
            (LOOKUP-VARIABLE-VALUE var (cdr env))
            (cdr binding)))))

; Needed to support rackunit tests in concrete-test.rkt.
(provide ASSQ
         LOOKUP-VARIABLE-VALUE)
