;;; $Id: chap7b.scm,v 4.0 1995/07/10 06:51:48 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Refinement of chap6d and chap7a. This interpreter introduces a
;;; *val* register and a *stack* to save/restore arguments that wait
;;; to be stored in an activation block. Functions now take their
;;; activation frame in the *val* register and temporarily stored in *fun*.

;;; Load chap6d before.

(define *val* #f)
(define *fun* #f)

(define *stack* (make-vector 1000))
(define *stack-index* 0)

(define (stack-push v)
  (vector-set! *stack* *stack-index* v)
  (set! *stack-index* (+ *stack-index* 1)) )

(define (stack-pop)
  (set! *stack-index* (- *stack-index* 1))
  (vector-ref *stack* *stack-index*) )

(define (save-stack)
  (let ((copy (make-vector *stack-index*)))
    (vector-copy! *stack* copy 0 *stack-index*)
    copy ) )

(define (restore-stack copy)
  (set! *stack-index* (vector-length copy))
  (vector-copy! copy *stack* 0 *stack-index*) )

;;; Copy vector old[start..end[ into vector new[start..end[
(define (vector-copy! old new start end)
  (let copy ((i start))
    (when (< i end)
          (vector-set! new i (vector-ref old i))
          (copy (+ i 1)) ) ) )

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
    (stack-push *val*)
    (m2)
    (set! *arg1* (stack-pop))
    (set! *val* (address *arg1* *val*)) ) )

(define (CALL3 address m1 m2 m3)
  (lambda ()  
    (m1)
    (stack-push *val*)
    (m2)
    (stack-push *val*)
    (m3)
    (set! *arg2* (stack-pop))
    (set! *arg1* (stack-pop))
    (set! *val* (address *arg1* *arg2* *val*)) ) )

(define (FIX-CLOSURE m+ arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (define (the-function sr)
        (if (= (activation-frame-argument-length *val*) arity+1)
            (begin (set! *env* (sr-extend* sr *val*))
                   (m+) )
            (wrong "Incorrect arity") ) )
      (set! *val* (make-closure the-function *env*)) ) ) )

(define (NARY-CLOSURE m+ arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (define (the-function sr)
        (if (>= (activation-frame-argument-length *val*) arity+1)
            (begin 
              (listify! *val* arity)
              (set! *env* (sr-extend* sr *val*))
              (m+) )
            (wrong "Incorrect arity") ) )
      (set! *val* (make-closure the-function *env*)) ) ) )

(define (TR-REGULAR-CALL m m*) 
  (lambda ()
    (m)
    (stack-push *val*)
    (m*)
    (set! *fun* (stack-pop))
    (invoke *fun*) ) )

(define (REGULAR-CALL m m*)
  (lambda ()
    (m)
    (stack-push *val*)
    (m*)
    (set! *fun* (stack-pop))
    (stack-push *env*)
    (invoke *fun*)
    (set! *env* (stack-pop)) ) )

(define (STORE-ARGUMENT m m* rank)
  (lambda ()
    (m)
    (stack-push *val*)
    (m*)
    (let ((v (stack-pop)))
      (set-activation-frame-argument! *val* rank v) ) ) )

(define (CONS-ARGUMENT m m* arity)
  (lambda ()
    (m)
    (stack-push *val*)
    (m*)
    (set-activation-frame-argument! 
     *val* arity (cons (stack-pop)
                       (activation-frame-argument *val* arity) ) ) ) )

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
;;; The activation frame is in the *val* register.

(define (invoke f)
  (if (closure? f)
      ((closure-code f) (closure-closed-environment f))
      (wrong "Not a function" f) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define-syntax defprimitive1
  (syntax-rules ()
    ((defprimitive1 name value)
     (definitial name
       (letrec ((arity+1 (+ 1 1))
                (behavior
                 (lambda (sr)
                   (if (= arity+1 (activation-frame-argument-length *val*))
                       (set! *val* (value (activation-frame-argument *val* 0)))
                       (wrong "Incorrect arity" 'name) ) ) ) )
         (description-extend! 'name `(function ,value a))
         (make-closure behavior sr.init) ) ) ) ) )
  
(define-syntax defprimitive2
  (syntax-rules ()
    ((defprimitive2 name value)
     (definitial name
       (letrec ((arity+1 (+ 2 1))
                (behavior
                 (lambda (sr)
                   (if (= arity+1 (activation-frame-argument-length *val*))
                       (set! *val* 
                             (value (activation-frame-argument *val* 0) 
                                    (activation-frame-argument *val* 1) ) )
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

;;; Must redefine this one with the new combinators.

(definitial list 
  (begin ((NARY-CLOSURE (SHALLOW-ARGUMENT-REF 0) 0))
         *val* ) )

(definitial call/cc
  (let* ((arity 1)
         (arity+1 (+ arity 1)) )
    (make-closure
     (lambda (sr)
       (if (= arity+1 (activation-frame-argument-length *val*))
           (call/cc
            (lambda (k)
              (let ((f (activation-frame-argument *val* 0))
                    (frame (allocate-activation-frame (+ 1 1)))
                    (stack-copy (save-stack)) )
                (set-activation-frame-argument! 
                 frame 0
                 (make-closure
                  (lambda (sr)
                    (if (= arity+1 (activation-frame-argument-length *val*))
                        (begin
                          (restore-stack stack-copy)
                          (set! *val* (activation-frame-argument *val* 0))
                          (k 'void) )
                        (wrong "Incorrect arity" 'continuation) ) )
                   sr.init ) )
                (set! *val* frame)
                (invoke f) ) ) )
           (wrong "Incorrect arity" 'call/cc) ) )
     sr.init ) ) )

(definitial apply
  (let* ((arity 2)
         (arity+1 (+ arity 1)) )
    (make-closure
     (lambda (sr)
       (if (>= (activation-frame-argument-length *val*) arity+1)
           (let* ((proc (activation-frame-argument *val* 0))
                  (last-arg-index (- (activation-frame-argument-length *val*) 2))
                  (last-arg (activation-frame-argument *val* last-arg-index))
                  (size (+ last-arg-index (length last-arg)))
                  (frame (allocate-activation-frame size)) )
             (do ((i 1 (+ i 1)))
                 ((= i last-arg-index))
               (set-activation-frame-argument! 
                frame (- i 1) (activation-frame-argument *val* i) ) )
             (do ((i (- last-arg-index 1) (+ i 1))
                  (last-arg last-arg (cdr last-arg)) )
                 ((null? last-arg))
               (set-activation-frame-argument! frame i (car last-arg)) )
             (set! *val* frame)
             (invoke proc) )
           (wrong "Incorrect arity" 'apply) ) )
     sr.init ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define install-regular-combinators
  (let ((originals (map eval combinator-names)))
    (lambda () 
      (for-each (lambda (old-value name)
                  (eval `(set! ,name ',old-value)) )
                originals
                combinator-names ) ) ) )

(define (chapter7b-interpreter)
  (define (toplevel)
    (set! *env* sr.init)
    (set! *val* #f)
    (set! *stack-index* 0)
    ((meaning (read) r.init #t))
    (display *val*)
    (toplevel) )
  (toplevel) )

(define (stand-alone-producer7b e)
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
      (set! *stack-index* 0)
      (m) ) ) )

(define (test-scheme7b file)
  (suite-test 
   file 
   "Scheme? " 
   "Scheme= "
   #t
   (lambda (read check error)
     (set! wrong error)
     (set! static-wrong error)
     (lambda ()
       ((stand-alone-producer7b (read)))
       (check *val*) ) )
   equal? ) )

;;; Other global variables.

(define *arg1* #f)

;;; end of chap7b.scm
