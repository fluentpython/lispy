;;; $Id: chap7c.scm,v 4.3 2006/11/24 18:16:37 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Refinement of chap6d and chap7b. This interpreter introduces a
;;; *val* register and a *stack* to save/restore arguments that wait
;;; to be stored in an activation block. Functions now take their
;;; activation frame in the *val* register. Code is now a list of combinators. 

;;; Load chap6d before.

(define *val* #f)

(define *fun* #f)
(define *arg1* #f)
(define *arg2* #f)

(define *pc* '())

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

(define-class primitive Object
  ( address ) )

(define-class continuation Object
  ( stack 
    ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Combinators

(define (SHALLOW-ARGUMENT-REF j)
  (list (lambda () (set! *val* (activation-frame-argument *env* j)))) )

(define (PREDEFINED i)
  (list (lambda () (set! *val* (predefined-fetch i)))) )

(define (DEEP-ARGUMENT-REF i j)
  (list (lambda () (set! *val* (deep-fetch *env* i j)))) )

(define (SHALLOW-ARGUMENT-SET! j m)
  (append m (SET-SHALLOW-ARGUMENT! j)) )

(define (SET-SHALLOW-ARGUMENT! j)
  (list (lambda () (set-activation-frame-argument! *env* j *val*))) )

(define (DEEP-ARGUMENT-SET! i j m)
  (append m (SET-DEEP-ARGUMENT! i j)) )

(define (SET-DEEP-ARGUMENT! i j)
  (list (lambda () (deep-update! *env* i j *val*))) )

(define (GLOBAL-REF i)
  (list (lambda () (set! *val* (global-fetch i)))) )

(define (CHECKED-GLOBAL-REF i)
  (list (lambda () (set! *val* (global-fetch i))
                   (when (eq? *val* undefined-value)
                     (wrong "Uninitialized variable") ))) )

(define (GLOBAL-SET! i m)
  (append m (SET-GLOBAL! i)) )

(define (SET-GLOBAL! i)
  (list (lambda () (global-update! i *val*))) )

(define (CONSTANT value)
  (list (lambda () (set! *val* value))) )

(define (ALTERNATIVE m1 m2 m3)
  (append m1 (JUMP-FALSE (+ 1 (length m2)))
          m2 (GOTO (length m3)) 
          m3 ) )

(define (JUMP-FALSE i)
  (list (lambda () (if (not *val*) (set! *pc* (list-tail *pc* i))))) )

(define (GOTO i)
  (list (lambda () (set! *pc* (list-tail *pc* i)))) )

(define (SEQUENCE m m+)
  (append m m+) )

(define (TR-FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+) )

(define (EXTEND-ENV)
  (list (lambda () (set! *env* (sr-extend* *env* *val*)))) )

(define (FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+ (UNLINK-ENV)) )

(define (UNLINK-ENV)
  (list (lambda () (set! *env* (activation-frame-next *env*)))) )

(define (CALL0 address)
  (list (lambda () (set! *val* (address)))) )

(define (CALL1 address m1)
  (append m1 (INVOKE1 address) ) )

(define (INVOKE1 address)
  (list (lambda () (set! *val* (address *val*)))) )

(define (CALL2 address m1 m2)
  (append m1 (PUSH-VALUE) m2 (POP-ARG1) (INVOKE2 address)) )

(define (PUSH-VALUE)
  (list (lambda () (stack-push *val*))) )

(define (POP-ARG1)
  (list (lambda () (set! *arg1* (stack-pop)))) )

(define (INVOKE2 address)
  (list (lambda () (set! *val* (address *arg1* *val*)))) )

(define (CALL3 address m1 m2 m3)
  (append m1 (PUSH-VALUE) 
          m2 (PUSH-VALUE) 
          m3 (POP-ARG2) (POP-ARG1) (INVOKE3 address) ) )

(define (POP-ARG2)
  (list (lambda () (set! *arg2* (stack-pop)))) )

(define (INVOKE3 address)
  (list (lambda () (set! *val* (address *arg1* *arg2* *val*)))) )

(define (FIX-CLOSURE m+ arity)
  (define the-function
    (append (ARITY=? (+ arity 1)) (EXTEND-ENV) m+ (RETURN)) )
  (append (CREATE-CLOSURE 1) (GOTO (length the-function)) 
          the-function ) )

(define (CREATE-CLOSURE offset)
  (list (lambda () (set! *val* (make-closure (list-tail *pc* offset) 
                                             *env* )))) )

(define (ARITY=? arity+1)
  (list (lambda () 
          (unless (= (activation-frame-argument-length *val*) arity+1)
            (wrong "Incorrect arity") ) )) )

(define (NARY-CLOSURE m+ arity)
  (define the-function
    (append (ARITY>=? (+ arity 1)) (PACK-FRAME! arity) (EXTEND-ENV)
            m+ (RETURN) ) )
  (append (CREATE-CLOSURE 1) (GOTO (length the-function)) 
          the-function ) )

(define (RETURN)
  (list (lambda () (set! *pc* (stack-pop)))) )

(define (PACK-FRAME! arity)
  (list (lambda () (listify! *val* arity))) )

(define (ARITY>=? arity+1)
  (list (lambda () 
          (unless (>= (activation-frame-argument-length *val*) arity+1)
            (wrong "Incorrect arity") ) )) )

(define (TR-REGULAR-CALL m m*)
  (append m (PUSH-VALUE) m* (POP-FUNCTION) (FUNCTION-INVOKE)) )

(define (POP-FUNCTION)
  (list (lambda () (set! *fun* (stack-pop)))) )

(define (FUNCTION-INVOKE)
  (list (lambda () (invoke *fun*))) )

(define (REGULAR-CALL m m*)
  (append m (PUSH-VALUE) 
          m* (POP-FUNCTION) (PRESERVE-ENV) 
             (FUNCTION-INVOKE) (RESTORE-ENV)
          ) )

(define (PRESERVE-ENV)
  (list (lambda () (stack-push *env*))) )

(define (RESTORE-ENV)
  (list (lambda () (set! *env* (stack-pop)))) )

(define (STORE-ARGUMENT m m* rank)
  (append m (PUSH-VALUE) m* (POP-FRAME! rank)) )

(define (POP-FRAME! rank)
  (list (lambda () (set-activation-frame-argument! *val* rank (stack-pop)))) )

(define (CONS-ARGUMENT m m* arity)
  (append m (PUSH-VALUE) m* (POP-CONS-FRAME! arity)) )

(define (POP-CONS-FRAME! arity)
  (list (lambda () 
          (set-activation-frame-argument! 
           *val* arity (cons (stack-pop)
                             (activation-frame-argument *val* arity) ) ) )) )

(define (ALLOCATE-FRAME size)
  (let ((size+1 (+ size 1)))
    (list (lambda () (set! *val* (allocate-activation-frame size+1)))) ) )

(define (ALLOCATE-DOTTED-FRAME arity)
  (let ((arity+1 (+ arity 1)))
    (list (lambda ()
            (let ((v* (allocate-activation-frame arity+1)))
              (set-activation-frame-argument! v* arity '())
              (set! *val* v*) ) )) ) )

(define (FINISH)
  (list (lambda () (*exit* *val*))) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define (invoke f)
  (cond ((closure? f)
         (stack-push *pc*)
         (set! *env* (closure-closed-environment f))
         (set! *pc* (closure-code f)) )
        ((primitive? f)
         ((primitive-address f)) )
        ((continuation? f)
         (if (= (+ 1 1) (activation-frame-argument-length *val*))
             (begin
               (restore-stack (continuation-stack f))
               (set! *val* (activation-frame-argument *val* 0))
               (set! *pc* (stack-pop)) )
             (wrong "Incorrect arity" 'continuation) ) )
        (else (wrong "Not a function" f)) ) )
      
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define-syntax defprimitive1
  (syntax-rules ()
    ((defprimitive1 name value)
     (definitial name
       (letrec ((arity+1 (+ 1 1))
                (behavior
                 (lambda ()
                   (if (= arity+1 (activation-frame-argument-length *val*))
                       (set! *val* (value (activation-frame-argument *val* 0)))
                       (wrong "Incorrect arity" 'name) ) ) ) )
         (description-extend! 'name `(function ,value a))
         (make-primitive behavior) ) ) ) ) )
  
(define-syntax defprimitive2
  (syntax-rules ()
    ((defprimitive2 name value)
     (definitial name
       (letrec ((arity+1 (+ 2 1))
                (behavior
                 (lambda ()
                   (show-registers 'name) ;; debug
                   (if (= arity+1 (activation-frame-argument-length *val*))
                       (set! *val* 
                             (value (activation-frame-argument *val* 0) 
                                    (activation-frame-argument *val* 1) ) )
                       (wrong "Incorrect arity" 'name) ) ) ) )
         (description-extend! 'name `(function ,value a b))
         (make-primitive behavior) ) ) ) ) )

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

(definitial call/cc
  (let* ((arity 1)
         (arity+1 (+ arity 1)) )
    (make-primitive
     (lambda ()
       (if (= arity+1 (activation-frame-argument-length *val*))
           (let ((f (activation-frame-argument *val* 0))
                 (frame (allocate-activation-frame (+ 1 1))))
             (stack-push *pc*)
             (set-activation-frame-argument! 
              frame 0 (make-continuation (save-stack)) )
             (stack-pop)
             (set! *val* frame)
             (invoke f) )
           (wrong "Incorrect arity" 'call/cc) ) ) ) ) )

(definitial apply
  (let* ((arity 2)
         (arity+1 (+ arity 1)) )
    (make-primitive
     (lambda ()
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
           (wrong "Incorrect arity" 'apply) ) ) ) ) )

(definitial list
  (make-primitive
   (lambda ()
     (let ((args-number (- (activation-frame-argument-length *val*) 1))
           (result '()) )
       (do ((i args-number (- i 1)))
           ((= i 0))
         (set! result (cons (activation-frame-argument *val* (- i 1)) 
                            result )) ) 
       (set! *val* result) ) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define *debug* #f)

(define (show-registers message)
  (when *debug* (format #t "
----------------~A
PC    = -~A
ENV   = ~A
VAL   = ~A
FUN   = ~A
STACK = ~A~%" message (length *pc*)
              *env* *val* *fun* (save-stack) ) )
  )

(define (run)
  (let ((instruction (car *pc*)))
    (set! *pc* (cdr *pc*))
    (instruction)
    (run) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


(define install-regular-combinators
  (let ((originals (map eval combinator-names)))
    (lambda () 
      (for-each (lambda (old-value name)
                  (eval `(set! ,name ',old-value)) )
                originals
                combinator-names ) ) ) )

(define (install-disassembling-combinators)
  (for-each (lambda (name)
              (eval `(set! ,name (lambda args (,name . ,args)))) )
            combinator-names ) )

(define combinator-names
  '( SHALLOW-ARGUMENT-REF
     PREDEFINED
     DEEP-ARGUMENT-REF
     SET-SHALLOW-ARGUMENT
     SET-DEEP-ARGUMENT!
     GLOBAL-REF
     CHECKED-GLOBAL-REF
     SET-GLOBAL!
     CONSTANT
     JUMP-FALSE
     GOTO
     EXTEND-ENV
     UNLINK-ENV
     CALL0
     INVOKE1
     PUSH-VALUE
     POP-ARG1
     INVOKE2
     POP-ARG2
     INVOKE3
     CREATE-CLOSURE
     ARITY=?
     RETURN
     PACK-FRAME!
     ARITY>=?
     POP-FUNCTION
     FUNCTION-INVOKE
     PRESERVE-ENV
     RESTORE-ENV
     POP-FRAME!
     POP-CONS-FRAME!
     ALLOCATE-FRAME
     ALLOCATE-DOTTED-FRAME
     FINISH
    ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define (chapter7c-interpreter)
  (define (toplevel)
    (define e (read))
    (set! *env* sr.init)
    (set! *val* #f)
    (set! *fun* #f)
    (set! *arg1* #f)
    (set! *arg2* #f)
    (set! *stack-index* 0)
    (set! *pc* (append (meaning e r.init #t)
                       (FINISH) ))
    (when *debug* (disassemble e) (display *pc*) (newline)) ;; DEBUG
    (call/cc (lambda (exit)
               (set! *exit* exit)
               (run) ))
    (display *val*)
    (toplevel) )
  (toplevel) )

(define (stand-alone-producer7c e)
  (set! g.current (original.g.current))
  (let* ((m (meaning e r.init #t))
         (size (length g.current))
         (global-names (map car (reverse g.current))) )
    (when *debug* (disassemble e)) ;; DEBUG
    (lambda ()
      (set! sg.current (make-vector size undefined-value))
      (set! sg.current.names global-names)
      (set! *env* sr.init)
      (set! *val* #f)
      (set! *fun* #f)
      (set! *arg1* #f)
      (set! *arg2* #f)
      (set! *stack-index* 0)
      (set! *pc* (append m (FINISH)))
      ;;(display m)(newline) ;; debug
      (call/cc (lambda (exit)
                 (set! *exit* exit)
                 (run) )) ) ) )    

(define (test-scheme7c file)
  (suite-test 
   file 
   "Scheme? " 
   "Scheme= "
   #t
   (lambda (read check error)
     (set! wrong error)
     (set! static-wrong error)
     (lambda ()
       ((stand-alone-producer7c (read)))
       (check *val*) ) )
   equal? ) )

;;; Missing definitions
(define *exit* #f)

;;; end of chap7c.scm
