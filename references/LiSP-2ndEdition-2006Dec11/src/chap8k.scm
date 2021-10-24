;;; $Id: chap8k.scm,v 4.0 1995/07/10 06:52:13 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Adaptation of the reflective interpreter to Scheme->C, Bigloo or
;;; SCM.  This is very messy, very hacky and poses a lot of problems
;;; with hygienic macros so expansion is done by hand. The three
;;; macros handle-location, capture-the-environment and
;;; the-environment are left here since they are exported to the book
;;; but they are useless.

(define-syntax handle-location
  (syntax-rules ()
    ((handle-location name)
     (lambda (value)
       (if (pair? value)
           (set! name (car value))
           name ) ) ) ) )

(define (expand-handle-location name)
  `(lambda (value) 
     (if (pair? value)
         (set! ,name (car value))
         ,name )) )

;;; This one poses problems since it assign lambda and monitor which
;;; are special forms.

(define-syntax capture-the-environment
  (syntax-rules ()
    ((capture-the-environment word ...)
     (lambda (name . value)
       (case name
         ((word) ((handle-location word) value)) ...
         ((display) (if (pair? value)
                        (wrong "Immutable" 'display)
                        show ))
         (else (if (pair? value)
                   (set-top-level-value! name (car value))
                   (top-level-value name) )) ) ) ) ) )

(define (expand-capture-the-environment . words)
  `(lambda (name . value)
     (case name
       ,@(map (lambda (w) `((,w) (,(expand-handle-location w) value)))
              words )
       ((display) (if (pair? value)
                      (wrong "Immutable" 'display)
                      show ))
       (else (if (pair? value)
                 (set-top-level-value! name (car value))
                 (top-level-value name) )) ) ) )

(define-syntax the-environment 
  (syntax-rules () 
    ((the-environment)
     (capture-the-environment make-toplevel make-flambda flambda?
      flambda-behavior prompt-in prompt-out exit it extend error
      global-env toplevel eval evlis eprogn reference quote if set!
      lambda flambda monitor ) ) ) )

(define this-environment
  (expand-capture-the-environment 'make-toplevel
                                  'make-flambda
                                  'flambda?
                                  'flambda-behavior
                                  'prompt-in
                                  'prompt-out
                                  'exit
                                  'it
                                  'extend
                                  'error
                                  'global-env
                                  'toplevel
                                  'eval
                                  'evlis
                                  'eprogn
                                  'reference
                                  'quote
                                  'if
                                  'set!
                                  'lambda
                                  'flambda
                                  'monitor ) )

;;; Use the underlying eval to access global variables.

(define (top-level-value name)
  (eval name) )

;;; Use define not to be bothered by inexistent variables.
;;; But gain warnings about redefinitions.

(define (set-top-level-value! name value)
  (eval `(define ,name ',value)) )

;;; Three ways (depending on the implementation) to simulate monitor.
;;; Fortunately, we do not need to simulate it exactly.

(define (bigloo-monitor handler body)
  (try (body)
       (lambda (k a b c)
         (display `(error**** ,a ,b ,c))(newline)
         (handler #f `(list ,a ,b ,c)) ) ) )

(define (scheme2c-monitor handler body)
  (letrec ((old-handler *error-handler*)
           (new-handler (lambda args
                          (display `(error**** . ,args))(newline)
                          (set! *error-handler* old-handler)
                          (handler #f args) )) )
    (set! *error-handler* new-handler)
    (let ((result (body)))
      (set! *error-handler* old-handler)
      result ) ) )

(define (scm-monitor handler body)
  (let ((result (catch-error (body))))
    (if (pair? result) (car result)
        (handler #f result) ) ) )

(define-syntax monitor
  (case book-interpreter-support
    ((bigloo)
     (syntax-rules ()
       ((monitor handler . body)
        (bigloo-monitor handler (lambda () . body)) ) )  )
    ((scheme2c)
     (syntax-rules ()
       ((monitor handler . body)
        (scheme2c-monitor handler (lambda () . body)) ) )  )
    ((scm)
     (syntax-rules ()
       ((monitor handler . body)
        (scm-monitor handler (lambda () . body)) ) ) )
    (else (display `(*** monitor simulation not supported ***))
          (newline)
          (/ 3 0) ) ) )

(define undefined (cons 'un 'defined))

(define-class Envir Object
  ( name value next ) )

(define (enrich env . names)
  (let enrich ((env env)(names names))
    (if (pair? names)
        (enrich (make-Envir (car names) undefined env) (cdr names))
        env ) ) )
  
(define (variable-defined? name env)
  (if (Envir? env)
      (or (eq? name (Envir-name env))
          (variable-defined? name (Envir-next env)) )
      #t ) )

(define (variable-value name env)
  (if (Envir? env)
      (if (eq? name (Envir-name env))
          (let ((value (Envir-value env)))
            (if (eq? value undefined)
                (error "Uninitialized variable" name)
                value ) )
          (variable-value name (Envir-next env)) )
      (env name) ) )

(define (set-variable-value! name env value)
  (if (Envir? env)
      (if (eq? name (Envir-name env))
          (set-Envir-value! env value)
          (set-variable-value! name (Envir-next env) value) )
      (env name value) ) )

(define t #t)
(define f #f)
(define nil '())

;;; Put the code of the reflective interpreter in this variable. This
;;; will be used by tests.
(define reflisp-code '())  

;;; Put the code which will be evaluated by the underlying Scheme evaluator
;;; here for debug purposes.
(define expanded-reflisp-code '())
(define coded-reflisp-code '())

;;; The problem is that two monitor forms appear in reflisp.scm and
;;; they must be expanded. The whole expression cannot be expanded
;;; since it contains a lambda whose variables have names that are the
;;; names of special forms and this is forbidden by syntax-case.
;;; This is a really big hack!

(define (expand-monitor-form e code)
  ;; receives (monitor) from the case form or (monitor h e ...)
  (cond ((null? (cdr e)) e)
        (else (code `(,(symbol-append book-interpreter-support
                                      '-monitor)
                      ,(cadr e) (lambda () . ,(cddr e)) )) ) ) )

(define reflisp
  (let ()
    (define (code e)
      (let ((v (assq e '( ))))
        (if (pair? v) (cdr v)
            (if (pair? e)
                (case (car e)
                  ((monitor) (expand-monitor-form e code))
                  ((the-environment) this-environment)
                  (else (cons (code (car e)) (code (cdr e)))) )
                e ) ) ) )
    (set! reflisp-code #f)
    (set! coded-reflisp-code #f)
    (set! expanded-reflisp-code #f)
    (let ((e (call-with-input-file "si/reflisp.scm"
               (lambda (port)
                 (let* ((e (read port))
                        (ne (code e)) )
                   (set! reflisp-code e)
                   (set! coded-reflisp-code ne)
                   ne ) ))))
      (set! expanded-reflisp-code e) ;;(pp e)   ; DEBUG
      ;; use the native eval
      (lambda () (eval e)) ) ) )

(define (count-pairs e)
  (if (pair? e)
      (+ 1 (count-pairs (car e)) (count-pairs (cdr e)))
      0 ) )

;;; end of chap8k.scm
