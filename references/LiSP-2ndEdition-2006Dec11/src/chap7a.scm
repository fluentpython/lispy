;;; $Id: chap7a.scm,v 4.0 1995/07/10 06:51:47 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Refinement of chap6d. This interpreter introduces a *val* register.

(define *val* #f)

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Combinators

(define (SHALLOW-ARGUMENT-REF j)
  (lambda () 
    (set! *val* (activation-frame-argument *env* j)) ) )

(define (PREDEFINED i)
  (lambda ()  
    (set! *val* (predefined-fetch i)) ) )

(define (DEEP-ARGUMENT-REF i j)
  (lambda () 
    (set! *val* (deep-fetch *env* i j)) ) )

(define (SHALLOW-ARGUMENT-SET! j m)
  (lambda () 
    (m)
    (set-activation-frame-argument! *env* j *val*) ) )

(define (DEEP-ARGUMENT-SET! i j m)
  (lambda () 
    (m)
    (deep-update! *env* i j *val*) ) )

(define (GLOBAL-REF i)
  (lambda () 
    (set! *val* (global-fetch i)) ) )

(define (CHECKED-GLOBAL-REF i)
  (lambda () 
    ((GLOBAL-REF i))
    (when (eq? *val* undefined-value)
      (wrong "Uninitialized variable") ) ) )

(define (GLOBAL-SET! i m)
  (lambda () 
    (m)
    (global-update! i *val*) ) )

(define (CONSTANT value)
  (lambda ()  
    (set! *val* value) ) )

(define (ALTERNATIVE m1 m2 m3)
  (lambda () 
    (m1)
    (if *val* (m2) (m3)) ) )

(define (SEQUENCE m m+)
  (lambda () (m) (m+)) )

(define (TR-FIX-LET m* m+)
  (lambda ()
    (m*)
    (set! *env* (sr-extend* *env* *val*))
    (m+) ) )

(define (FIX-LET m* m+)
  (lambda ()
    (m*)
    (set! *env* (sr-extend* *env* *val*))
    (m+)
    (set! *env* (activation-frame-next *env*)) ) )

(define (CALL0 address)
  (lambda ()  
    (set! *val* (address)) ) )

(define (CALL1 address m1)
  (lambda ()  
    (m1)
    (set! *val* (address *val*)) ) )

(define (CALL2 address m1 m2)
  (lambda ()  
    (m1)
    (let ((arg1 *val*))
      (m2)
      (set! *val* (address arg1 *val*)) ) ) )

(define (CALL3 address m1 m2 m3)
  (lambda ()  
    (m1)
    (let ((arg1 *val*))
      (m2)
      (let ((arg *val*))
        (m3)
        (set! *val* (address arg1 arg2 *val*)) ) ) ) )

(define (FIX-CLOSURE m+ arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (define (the-function v* sr)
        (if (= (activation-frame-argument-length v*) arity+1)
            (begin (set! *env* (sr-extend* sr v*))
                   (m+) )
            (wrong "Incorrect arity") ) )
    (set! *val* (make-closure the-function *env*)) ) ) )

(define (NARY-CLOSURE m+ arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (define (the-function v* sr)
        (if (>= (activation-frame-argument-length v*) arity+1)
            (begin 
              (listify! v* arity)
              (set! *env* (sr-extend* sr v*))
              (m+) )
            (wrong "Incorrect arity") ) )
      (set! *val* (make-closure the-function *env*)) ) ) )

(define (TR-REGULAR-CALL m m*) 
  (lambda ()
    (m)
    (let ((f *val*))
      (m*)
      (invoke f *val*) ) ) )

(define (REGULAR-CALL m m*)
  (lambda ()
    (m)
    (let ((f *val*))
      (m*)
      (let ((sr *env*))
        (invoke f *val*)
        (set! *env* sr) ) ) ) )

(define (STORE-ARGUMENT m m* rank)
  (lambda ()
    (m)
    (let ((v *val*))
      (m*)
      (set-activation-frame-argument! *val* rank v) ) ) )

(define (CONS-ARGUMENT m m* arity)
  (lambda ()
    (m)
    (let* ((v *val*))
      (m*)
      (set-activation-frame-argument! 
       *val* arity (cons v (activation-frame-argument *val* arity)) ) ) ) )

(define (ALLOCATE-FRAME size)
  (let ((size+1 (+ size 1)))
    (lambda ()
      (set! *val* (allocate-activation-frame size+1)) ) ) )

(define (ALLOCATE-DOTTED-FRAME arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (let ((v* (allocate-activation-frame arity+1)))
        (set-activation-frame-argument! v* arity '())
        (set! *val* v*) ) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define-syntax defprimitive1
  (syntax-rules ()
    ((defprimitive1 name value)
     (definitial name
       (letrec ((arity+1 (+ 1 1))
                (behavior
                 (lambda (v* sr)
                   (if (= arity+1 (activation-frame-argument-length v*))
                       (set! *val* (value (activation-frame-argument v* 0)))
                       (wrong "Incorrect arity" 'name) ) ) ) )
         (description-extend! 'name `(function ,value a))
         (make-closure behavior sr.init) ) ) ) ) )
  
(define-syntax defprimitive2
  (syntax-rules ()
    ((defprimitive2 name value)
     (definitial name
       (letrec ((arity+1 (+ 2 1))
                (behavior
                 (lambda (v* sr)
                   (if (= arity+1 (activation-frame-argument-length v*))
                       (set! *val* (value (activation-frame-argument v* 0) 
                                          (activation-frame-argument v* 1) ))
                       (wrong "Incorrect arity" 'name) ) ) ) )
         (description-extend! 'name `(function ,value a b))
         (make-closure behavior sr.init) ) ) ) ) )

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defprimitive pair? pair? 1)
(defprimitive symbol? symbol? 1)
(defprimitive eq? eq? 2)
(defprimitive set-car! set-car! 2)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defprimitive = = 2)
(defprimitive < < 2)
(defprimitive > > 2)
(defprimitive * * 2)
(defprimitive <= <= 2)
(defprimitive >= >= 2)
(defprimitive remainder remainder 2)
(defprimitive display display 1)

(definitial list 
  (begin ((NARY-CLOSURE (SHALLOW-ARGUMENT-REF 0) 0))
         *val* ) )

(definitial call/cc
  (let* ((arity 1)
         (arity+1 (+ arity 1)) )
    (make-closure
     (lambda (v* sr)
       (if (= arity+1 (activation-frame-argument-length v*))
           (call/cc
            (lambda (k)
              (invoke
               (activation-frame-argument v* 0)
               (let ((frame (allocate-activation-frame (+ 1 1))))
                 (set-activation-frame-argument! 
                  frame 0
                  (make-closure
                   (lambda (values r)
                     (if (= arity+1 (activation-frame-argument-length values))
                         (begin
                           (set! *val* (activation-frame-argument values 0))
                           (k 'void) )
                         (wrong "Incorrect arity" 'continuation) ) )
                   sr.init ) )
                 frame ) ) ) )
           (wrong "Incorrect arity" 'call/cc) ) )
     sr.init ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define install-regular-combinators
  (let ((originals (map eval combinator-names)))
    (lambda () 
      (for-each (lambda (old-value name)
                  (eval `(set! ,name ',old-value)) )
                originals
                combinator-names ) ) ) )

(define (chapter7a-interpreter)
  (define (toplevel)
    (set! *env* sr.init)
    (set! *val* #f)
    ((meaning (read) r.init #t))
    (display *val*)
    (toplevel) )
  (toplevel) )

(define (stand-alone-producer7a e)
  (set! g.current (original.g.current))
  (let* ((m (meaning e r.init #t))
         (size (length g.current))
         (global-names (map car (reverse g.current))) )
    ;(disassemble e) ;; DEBUG
    (lambda ()
      (set! sg.current (make-vector size undefined-value))
      (set! sg.current.names global-names)
      (set! *env* sr.init)
      (set! *val* #f)
      (m) ) ) )

(define (test-scheme7a file)
  (suite-test 
   file 
   "Scheme? " 
   "Scheme= "
   #t
   (lambda (read check error)
     (set! wrong error)
     (set! static-wrong error)
     (lambda ()
       ((stand-alone-producer7a (read)))
       (check *val*) ) )
   equal? ) )

;;; end of chap7a.scm
