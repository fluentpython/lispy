;;; output.ss
;;; Robert Hieb & Kent Dybvig
;;; 92/06/18

; The output routines can be tailored to feed a specific system or compiler.
; They are set up here to generate the following subset of standard Scheme:

;  <expression> :== <application>
;                |  <variable>
;                |  (set! <variable> <expression>)
;                |  (define <variable> <expression>)
;                |  (lambda (<variable>*) <expression>)
;                |  (lambda <variable> <expression>)
;                |  (lambda (<variable>+ . <variable>) <expression>)
;                |  (letrec (<binding>+) <expression>)
;                |  (if <expression> <expression> <expression>)
;                |  (begin <expression> <expression>)
;                |  (quote <datum>)
; <application> :== (<expression>+)
;     <binding> :== (<variable> <expression>)
;    <variable> :== <symbol>

; Definitions are generated only at top level.

(define build-application
   (lambda (fun-exp arg-exps)
      `(,fun-exp ,@arg-exps)))

(define build-conditional
   (lambda (test-exp then-exp else-exp)
      `(if ,test-exp ,then-exp ,else-exp)))

(define build-lexical-reference (lambda (var) var))

(define build-lexical-assignment
   (lambda (var exp)
      `(set! ,var ,exp)))

(define build-global-reference (lambda (var) var))

(define build-global-assignment
   (lambda (var exp)
      `(set! ,var ,exp)))

(define build-lambda
   (lambda (vars exp)
      `(lambda ,vars ,exp)))

(define build-improper-lambda
   (lambda (vars var exp)
      `(lambda (,@vars . ,var) ,exp)))

(define build-data
   (lambda (exp)
      `(quote ,exp)))

(define build-identifier
   (lambda (id)
      `(quote ,id)))

(define build-sequence
   (lambda (exps)
      (if (null? (cdr exps))
          (car exps)
          `(begin ,(car exps) ,(build-sequence (cdr exps))))))

(define build-letrec
   (lambda (vars val-exps body-exp)
      (if (null? vars)
          body-exp
          `(letrec ,(map list vars val-exps) ,body-exp))))

(define build-global-definition
   (lambda (var val)
      `(define ,var ,val)))
