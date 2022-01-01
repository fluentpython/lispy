#lang racket/base

; code adapted from the "evaluator with concrete syntax"
; listed on pages 121-122 of the "Instructor's Manual to
; accompany Structure and Interpretation of Computer Programs"
; by Julie Sussman with Harold Abelson and Gerald Jay Sussman.

; Tested with Racket 8.3.
; Function names changed to ALL-CAPS to avoid potential
; confusion with functions provided by Racket.

(define (EVAL exp environ)
  ;; dispatch on expression type
  (cond [(number? exp) exp]                    ; self-evaluating
        [(symbol? exp)                         ; variable?
         (LOOKUP-VARIABLE-VALUE exp environ)]
        [(eq? (car exp) 'QUOTE) (cadr exp)]    ; quoted?
        ;; rule for procedure objects in the environment model
        [(eq? (car exp) 'LAMBDA)               ; lambda?
         (list 'procedure                      ; make-procedure
               (cadr exp)                      ; lambda-parameters
               (caddr exp)                     ; lambda-body
               environ)]
        [(eq? (car exp) 'IF)                   ; if?
         (EVAL-IF exp environ)]
        [else
         (APPLY (EVAL (car exp) environ)       ; operator
                (LIST-OF-VALUES (cdr exp)      ; operands
                                environ))]))

(define (APPLY procedure arguments)
  ;; dispatch on procedure type
  (cond [(and (list? procedure) (eq? (car procedure) 'procedure)) ; compoud-procedure?
         (EVAL
          (caddr procedure)       ; procedure-body
          (EXTEND-ENVIRONMENT
           (cadr procedure)       ; procedure-parameters
           arguments 
           (cadddr procedure)))]  ; procedure-environment
  [else (apply procedure arguments)])) ; (apply ...) from host interpreter

(define (LIST-OF-VALUES exps environ)
  (if (null? exps)
      '()
      (cons (EVAL (car exps) environ)
            (LIST-OF-VALUES (cdr exps) environ))))

(define (EVAL-IF exp environ)
  (if (EVAL (cadr exp) environ)      ; if-predicate
      (EVAL (caddr exp) environ)         ; if-consequent
      (EVAL (cadddr exp) environ)))  ; if-alternative

(define (EXTEND-ENVIRONMENT vars values base-environ)
  (cons (MAKE-FRAME vars values) base-environ))

(define (MAKE-FRAME vars values)
  (cond [(and (null? vars) (null? values)) '()]
        [(null? vars) (error "Too many arguments" values)]
        [(null? values) (error "Too few arguments" vars)]
        [else
         (cons (cons (car vars) (car values)) ; make binding
               (MAKE-FRAME (cdr vars) (cdr values)))]))
        
(define (LOOKUP-VARIABLE-VALUE var environ)
  (if (null? environ)
      (error "Unbound variable" var)
      (let ([binding (ASSQ var (car environ))]) ; look in first frame
        (if (eq? binding #f)
            (LOOKUP-VARIABLE-VALUE var (cdr environ))
            (cdr binding)))))

(define (ASSQ var bindings)
  (cond [(null? bindings) #f]
        [(eq? var (caar bindings)) (car bindings)]
        [else (ASSQ var (cdr bindings))]))

(define (MAKE-BUILTINS)
  (EXTEND-ENVIRONMENT
   '(+ - * / <)
   (list + - * / <)
   '()))

; Needed to support rackunit tests in concrete-test.rkt.
(provide ASSQ
         LOOKUP-VARIABLE-VALUE
         EXTEND-ENVIRONMENT
         MAKE-FRAME
         EVAL
         MAKE-BUILTINS)
