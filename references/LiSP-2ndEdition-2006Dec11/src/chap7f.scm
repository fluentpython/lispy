;;; $Id: chap7f.scm,v 4.2 2006/11/27 13:29:34 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Instruction set.
;;; This file is read by chap7d.scm

(define-instruction (SHALLOW-ARGUMENT-REF0) 1 
  (set! *val* (activation-frame-argument *env* 0)) )

(define-instruction (SHALLOW-ARGUMENT-REF1) 2 
  (set! *val* (activation-frame-argument *env* 1)) )

(define-instruction (SHALLOW-ARGUMENT-REF2) 3 
  (set! *val* (activation-frame-argument *env* 2)) )

(define-instruction (SHALLOW-ARGUMENT-REF3) 4 
  (set! *val* (activation-frame-argument *env* 3)) )

(define-instruction (SHALLOW-ARGUMENT-REF j) 5 
  (set! *val* (activation-frame-argument *env* j)) )

(define-instruction (DEEP-ARGUMENT-REF i j) 6 
  (set! *val* (deep-fetch *env* i j)) )

(define-instruction (GLOBAL-REF i) 7 
  (set! *val* (global-fetch i)) )

;(define-instruction (CHECKED-GLOBAL-REF i) 8 
;  (set! *val* (global-fetch i))
;  (if (eq? *val* undefined-value)
;    (signal-exception #t (list "Uninitialized global variable" i))
;    (vector-set! *code* (- *pc* 2) 7) ) ) 

(define-instruction (CHECKED-GLOBAL-REF i) 8 
  (set! *val* (global-fetch i))
  (when (eq? *val* undefined-value)
    (signal-exception 
     #t (list "Uninitialized global variable" i) ) ) )

(define-instruction (CONSTANT i) 9 
  (set! *val* (quotation-fetch i)) )

(define-instruction (PREDEFINED0) 10   ; \#T 
  (set! *val* #t) )

(define-instruction (PREDEFINED1) 11   ; \#F 
  (set! *val* #f) )

(define-instruction (PREDEFINED2) 12   ; () 
  (set! *val* '()) )

(define-instruction (PREDEFINED3) 13   ; cons 
  (set! *val* (predefined-fetch 3)) )

(define-instruction (PREDEFINED4) 14   ; car 
  (set! *val* (predefined-fetch 4)) )

(define-instruction (PREDEFINED5) 15   ; cdr
  (set! *val* (predefined-fetch 5)) )

(define-instruction (PREDEFINED6) 16   ; pair? 
  (set! *val* (predefined-fetch 6)) )

(define-instruction (PREDEFINED7) 17   ; symbol? 
  (set! *val* (predefined-fetch 7)) )

(define-instruction (PREDEFINED8) 18   ; eq? 
  (set! *val* (predefined-fetch 8)) )

(define-instruction (PREDEFINED i) 19 
  (set! *val* (predefined-fetch i)) )

(define-instruction (FINISH) 20 
  (*exit* *val*) )

(define-instruction (SET-SHALLOW-ARGUMENT!0) 21 
  (set-activation-frame-argument! *env* 0 *val*) )

(define-instruction (SET-SHALLOW-ARGUMENT!1) 22 
  (set-activation-frame-argument! *env* 1 *val*) )

(define-instruction (SET-SHALLOW-ARGUMENT!2) 23 
  (set-activation-frame-argument! *env* 2 *val*) )

(define-instruction (SET-SHALLOW-ARGUMENT!3) 24 
  (set-activation-frame-argument! *env* 3 *val*) )

(define-instruction (SET-SHALLOW-ARGUMENT! j) 25 
  (set-activation-frame-argument! *env* j *val*) )

(define-instruction (SET-DEEP-ARGUMENT! i j) 26 
  (deep-update! *env* i j *val*) )

(define-instruction (SET-GLOBAL! i) 27 
  (global-update! i *val*) )

(define-instruction (LONG-GOTO offset1 offset2) 28 
  (let ((offset (+ offset1 (* 256 offset2))) )
    (set! *pc* (+ *pc* offset)) ) )

(define-instruction (LONG-JUMP-FALSE offset1 offset2) 29 
  (let ((offset (+ offset1 (* 256 offset2))) )
    (if (not *val*) (set! *pc* (+ *pc* offset))) ) )

(define-instruction (SHORT-GOTO offset) 30 
  (set! *pc* (+ *pc* offset)) )

(define-instruction (SHORT-JUMP-FALSE offset) 31 
  (if (not *val*) (set! *pc* (+ *pc* offset))) )

(define-instruction (EXTEND-ENV) 32 
  (set! *env* (sr-extend* *env* *val*)) )

(define-instruction (UNLINK-ENV) 33 
  (set! *env* (activation-frame-next *env*)) )

(define-instruction (PUSH-VALUE) 34 
  (stack-push *val*) )

(define-instruction (POP-ARG1) 35 
  (set! *arg1* (stack-pop)) )

(define-instruction (POP-ARG2) 36 
  (set! *arg2* (stack-pop)) )

(define-instruction (PRESERVE-ENV) 37 
  (preserve-environment) )

(define-instruction (RESTORE-ENV) 38 
  (restore-environment) )

(define-instruction (POP-FUNCTION) 39 
  (set! *fun* (stack-pop)) )

(define-instruction (CREATE-CLOSURE offset) 40 
  (set! *val* (make-closure (+ *pc* offset) *env*)) )

(define-instruction (RETURN) 43 
  (set! *pc* (stack-pop)) )

(define-instruction (PACK-FRAME! arity) 44 
  (listify! *val* arity) )

(define-instruction (FUNCTION-INVOKE) 45 
  (invoke *fun* #f) )

(define-instruction (FUNCTION-GOTO) 46 
  (invoke *fun* #t) )

(define-instruction (POP-CONS-FRAME! arity) 47 
  (set-activation-frame-argument! 
   *val* arity (cons (stack-pop)
                     (activation-frame-argument *val* arity) ) ) )

(define-instruction (ALLOCATE-FRAME1) 50 
  (set! *val* (allocate-activation-frame 1)) )

(define-instruction (ALLOCATE-FRAME2) 51 
  (set! *val* (allocate-activation-frame 2)) )

(define-instruction (ALLOCATE-FRAME3) 52 
  (set! *val* (allocate-activation-frame 3)) )

(define-instruction (ALLOCATE-FRAME4) 53 
  (set! *val* (allocate-activation-frame 4)) )

(define-instruction (ALLOCATE-FRAME5) 54
  (set! *val* (allocate-activation-frame 5)) )

(define-instruction (ALLOCATE-FRAME size+1) 55
  (set! *val* (allocate-activation-frame size+1)) )

(define-instruction (ALLOCATE-DOTTED-FRAME arity) 56 
  (let ((v* (allocate-activation-frame arity)))
    (set-activation-frame-argument! v* (- arity 1) '())
    (set! *val* v*) ) )

(define-instruction (POP-FRAME!0) 60 
  (set-activation-frame-argument! *val* 0 (stack-pop)) )

(define-instruction (POP-FRAME!1) 61 
  (set-activation-frame-argument! *val* 1 (stack-pop)) )

(define-instruction (POP-FRAME!2) 62 
  (set-activation-frame-argument! *val* 2 (stack-pop)) )

(define-instruction (POP-FRAME!3) 63 
  (set-activation-frame-argument! *val* 3 (stack-pop)) )

(define-instruction (POP-FRAME! rank) 64 
  (set-activation-frame-argument! *val* rank (stack-pop)) )

(define-instruction (ARITY=?1) 71 
  (unless (= (activation-frame-argument-length *val*) 1)
    (signal-exception #f (list "Too much arguments for a thunk")) ) )

(define-instruction (ARITY=?2) 72 
  (unless (= (activation-frame-argument-length *val*) 2)
    (signal-exception 
     #f (list "Incorrect arity for unary function") ) ) )

(define-instruction (ARITY=?3) 73 
  (unless (= (activation-frame-argument-length *val*) 3)
    (signal-exception #f (list "Incorrect arity for binary function")) ) )

(define-instruction (ARITY=?4) 74 
  (unless (= (activation-frame-argument-length *val*) 4)
    (signal-exception #f (list "Incorrect arity for ternary function")) ) )

(define-instruction (ARITY=? arity+1) 75 
  (unless (= (activation-frame-argument-length *val*) arity+1)
    (signal-exception #f (list "Incorrect arity")) ) )

(define-instruction (ARITY>=? arity+1) 78 
  (unless (>= (activation-frame-argument-length *val*) arity+1)
    (signal-exception #f (list "Too less arguments for a nary function")) ) )

(define-instruction (SHORT-NUMBER value) 79 
  (set! *val* value) )

(define-instruction (CONSTANT-1)  80 
  (set! *val* -1) )
 
(define-instruction (CONSTANT0) 81 
  (set! *val* 0) )

(define-instruction (CONSTANT1) 82 
  (set! *val* 1) )

(define-instruction (CONSTANT2) 83 
  (set! *val* 2) )

(define-instruction (CONSTANT4) 84 
  (set! *val* 4) )

(define-instruction (CALL0-newline) 88
  (set! *val* (newline)) )

(define-instruction (CALL0-read) 89
  (set! *val* (read)) )

(define-instruction (CALL1-car) 90 
  (set! *val* (car *val*)) )

(define-instruction (CALL1-cdr) 91 
  (set! *val* (cdr *val*)) )

(define-instruction (CALL1-pair?) 92 
  (set! *val* (pair? *val*)) )

(define-instruction (CALL1-symbol?) 93 
  (set! *val* (symbol? *val*)) )

(define-instruction (CALL1-display) 94 
  (set! *val* (show *val*)) )

(define-instruction (CALL1-primitive?) 95
  (set! *val* (primitive? *val*)) )

(define-instruction (CALL1-null?) 96
  (set! *val* (null? *val*)) )

(define-instruction (CALL1-continuation?) 97
  (set! *val* (continuation? *val*)) )

(define-instruction (CALL1-eof-object?) 98
  (set! *val* (eof-object? *val*)) )

(define-instruction (CALL2-cons) 100 
  (set! *val* (cons *arg1* *val*)) )

(define-instruction (CALL2-eq?) 101 
  (set! *val* (eq? *arg1* *val*)) )

(define-instruction (CALL2-set-car!) 102 
  (set! *val* (set-car! *arg1* *val*)) )

(define-instruction (CALL2-set-cdr!) 103 
  (set! *val* (set-cdr! *arg1* *val*)) )

(define-instruction (CALL2-+) 104 
  (set! *val* (+ *arg1* *val*)) )

(define-instruction (CALL2--) 105 
  (set! *val* (- *arg1* *val*)) )

(define-instruction (CALL2-=) 106 
  (set! *val* (= *arg1* *val*)) )

(define-instruction (CALL2-<) 107 
  (set! *val* (< *arg1* *val*)) )

(define-instruction (CALL2->) 108 
  (set! *val* (> *arg1* *val*)) )

(define-instruction (CALL2-*) 109 
  (set! *val* (* *arg1* *val*)) )

(define-instruction (CALL2-<=) 110 
  (set! *val* (<= *arg1* *val*)) )

(define-instruction (CALL2->=) 111 
  (set! *val* (>= *arg1* *val*)) )

(define-instruction (CALL2-remainder) 112 
  (set! *val* (remainder *arg1* *val*)) )

(define-instruction (DYNAMIC-REF index) 240
  (set! *val* (find-dynamic-value index)) )

(define-instruction (DYNAMIC-POP) 241 
  (pop-dynamic-binding) )

(define-instruction (DYNAMIC-PUSH index) 242 
  (push-dynamic-binding index *val*) )

(define-instruction (NON-CONT-ERR) 245 
  (signal-exception #f (list "Non continuable exception continued")) )

(define-instruction (PUSH-HANDLER) 246 
  (push-exception-handler) )

(define-instruction (POP-HANDLER) 247 
  (pop-exception-handler) )

(define-instruction (POP-ESCAPER) 250 
  (let* ((tag (stack-pop))
         (escape (stack-pop)) )
    (restore-environment) ) )

(define-instruction (PUSH-ESCAPER offset) 251 
  (preserve-environment)
  (let* ((escape (make-escape (+ *stack-index* 3)))
         (frame (allocate-activation-frame 1)) )
    (set-activation-frame-argument! frame 0 escape)
    (set! *env* (sr-extend* *env* frame))
    (stack-push escape)
    (stack-push escape-tag)
    (stack-push (+ *pc* offset)) ) )

;;; Used by chap8d.scm (eval as a special form)

(define-instruction (COMPILE-RUN) 255
  (let ((v *val*)
        (r (stack-pop)) )
    (if (program? v)
        (compile-and-run v r #f)
        (signal-exception #t (list "Illegal program" v)) ) ) )

;;; Used by chap8h.scm (export special form)

(define-instruction (CREATE-1ST-CLASS-ENV) 254
  (create-first-class-environment *val* *env*) )

(define-instruction (CHECKED-DEEP-REF i j) 253
  (set! *val* (deep-fetch *env* i j))
  (when (eq? *val* undefined-value)
    (signal-exception #t (list "Uninitialized local variable")) ) )

(define-instruction (CREATE-PSEUDO-ENV) 252
  (create-pseudo-environment (stack-pop) *val* *env*) )

(define-instruction (SHADOW-REF i j) 231
  (shadowable-fetch *env* i j) )

(define-instruction (SET-SHADOW! i j) 232
  (shadowable-update! *env* i j *val*) )

;;; end of chap7f.scm
