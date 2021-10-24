;;; $Id: chap6e.scm,v 4.0 1995/07/10 06:51:41 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;;       A small bytecode compiler 
;;;   (in fact `byte' is a unappropriate word since it is more alike a
;;;    vectorized encoding). Most of the code comes from chap6d.scm
;;;    Changed sections have this comment ;;;oooooNEW as header.

;;;                      Threaded interpreter.
;;; Environment is held by a global variable. This is bad for //ism.
;;; Continuation are now implicit and call/cc is a magical operator.
;;; Also try to introduce combinators as much as possible.
;;; Closures are explicitely represented.
;;; Code is stored in a vector.

;;; Load chap6d.scm before

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The bytecode assembler

;;; There is a unique byte-code vector containing all code. A single integer
;;; (a program counter) allows to refer to some code. The first initial
;;; possible index is held in *byte-index*..

(define *byte-vector* 'wait)
(define *byte-index* 0)

(define (byte-compile e)
  (set! *byte-index* 0)
  (set! *byte-vector* (make-vector 50))
  (meaning e sr.init #t) )

(define (bytes-emit . bytes)
  (let ((pc *byte-index*)
        (n (length bytes)) )
    (when (>= (+ n *byte-index*) (vector-length *byte-vector*))
      (set! *byte-vector* (vector-extend *byte-vector*)) )
    (do ((bytes bytes (cdr bytes)))
        ((null? bytes))
      (vector-set! *byte-vector* *byte-index* (car bytes))
      (set! *byte-index* (+ *byte-index* 1)) )
    pc ) )

;;; These to functions come from Meroon.
;;; Return a new bigger vector. If the second argument is present then
;;; the returned vector is at least that long.
(define (vector-extend s . at-least)
  (let* ((n (vector-length s))
         (r (make-vector (if (pair? at-least)
                             (max (car at-least) n)
                             (+ n 1 (quotient n 2)) )
                         #f )) )
    (vector-copy! s r 0 n)
    r ) )

;;; Copy vector old[start..end[ into vector new[start..end[
(define (vector-copy! old new start end)
  (let copy ((i start))
    (when (< i end)
          (vector-set! new i (vector-ref old i))
          (copy (+ i 1)) ) ) )

(define (SHALLOW-ARGUMENT-REF j)
  (bytes-emit 0 j) )

(define (PREDEFINED i)
  (bytes-emit 12 i) )

(define (DEEP-ARGUMENT-REF i j)
  (bytes-emit 1 i j) )

(define (SHALLOW-ARGUMENT-SET! j m)
  (bytes-emit 2 j m) )

(define (DEEP-ARGUMENT-SET! i j m)
  (bytes-emit 3 i j m) )

(define (GLOBAL-REF i)
  (bytes-emit 4 i) )

(define (CHECKED-GLOBAL-REF i)
  (bytes-emit 5 i) )

(define (GLOBAL-SET! i m)
  (bytes-emit 6 i m) )

(define (CONSTANT value)
  (bytes-emit 7 value) )

(define (ALTERNATIVE m1 m2 m3)
  (bytes-emit 8 m1 m2 m3) )

(define (SEQUENCE m m+)
  (bytes-emit 9 m m+) )

(define (TR-FIX-LET m* m+)
  (bytes-emit 10 m* m+) )

(define (FIX-LET m* m+)
  (bytes-emit 11 m* m+) )

(define (CALL0 address)
  (bytes-emit 14 address) )

(define (CALL1 address m1)
  (bytes-emit 15 address m1) )

(define (CALL2 address m1 m2)
  (bytes-emit 16 address m1 m2) )

(define (CALL3 address m1 m2 m3)
  (bytes-emit 17 address m1 m2 m3) )

(define (FIX-CLOSURE m+ arity)
  (bytes-emit 18 m+ (+ 1 arity)) )

(define (NARY-CLOSURE m+ arity)
  (bytes-emit 19 m+ (+ 1 arity)) )

(define (TR-REGULAR-CALL m m*) 
  (bytes-emit 20 m m*) )

(define (REGULAR-CALL m m*)
  (bytes-emit 21 m m*) )

(define (STORE-ARGUMENT m m* rank)
  (bytes-emit 22 m m* rank) )

(define (ALLOCATE-FRAME size)
  (bytes-emit 23 (+ 1 size)) )

(define (CONS-ARGUMENT m m* rank)
  (bytes-emit 24 m m* rank) )

(define (ALLOCATE-DOTTED-FRAME arity)
  (bytes-emit 25 arity) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooNEW
;;; The bytecode runtime library

(define (create-fix-closure m+ arity+1)
  (define (the-function v* sr)
    (if (= (activation-frame-argument-length v*) arity+1)
        (begin (set! *env* (sr-extend* sr v*))
               (byte-eval m+) )
        (wrong "Incorrect arity") ) )
  (make-closure the-function *env*) )

(define (create-nary-closure m+ arity+1)
  (define arity (- arity+1 1))
  (define (the-function v* sr)
    (if (>= (activation-frame-argument-length v*) arity+1)
        (begin 
          (listify! v* arity)
          (set! *env* (sr-extend* sr v*))
          (byte-eval m+) )
        (wrong "Incorrect arity") ) )
  (make-closure the-function *env*) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooNEW
;;; The bytecode interpreter

(define (byte-eval pc)
  (let ((byte (vector-ref *byte-vector* pc)))
    (case byte
      ((0) ; SHALLOW-ARGUMENT-REF
       (let* ((j (vector-ref *byte-vector* (+ pc 1))))
         (activation-frame-argument *env* j) ) )
      ((1) ; DEEP-ARGUMENT-REF
       (let* ((i (vector-ref *byte-vector* (+ pc 1)))
              (j (vector-ref *byte-vector* (+ pc 2))) )
         (deep-fetch *env* i j) ) )
      ((2) ; SHALLOW-ARGUMENT-SET!
       (let* ((j (vector-ref *byte-vector* (+ pc 1)))
              (m (vector-ref *byte-vector* (+ pc 2)))
              (v (byte-eval m)) )
         (set-activation-frame-argument! *env* j v) ) )
      ((3) ; DEEP-ARGUMENT-SET!
       (let* ((i (vector-ref *byte-vector* (+ pc 1)))
              (j (vector-ref *byte-vector* (+ pc 2)))
              (m (vector-ref *byte-vector* (+ pc 3)))
              (v (byte-eval m)) )
         (deep-update! *env* i j v) ) )
      ((4) ; GLOBAL-REF
       (let* ((i (vector-ref *byte-vector* (+ pc 1))))
         (global-fetch i) ) )
      ((5) ; CHECKED-GLOBAL-REF
       (let* ((i (vector-ref *byte-vector* (+ pc 1))))
         (let ((v (global-fetch i)))
           (if (eq? v undefined-value)
               (wrong "Uninitialized variable")
               v ) ) ) )
      ((12) ; PREDEFINED
       (let ((i (vector-ref *byte-vector* (+ pc 1))))
         (let ((v (predefined-fetch i)))
           (if (eq? v undefined-value)
               (wrong "Uninitialized variable")
               v ) ) ) )
      ((6) ; GLOBAL-SET!
       (let* ((i (vector-ref *byte-vector* (+ pc 1)))
              (m (vector-ref *byte-vector* (+ pc 2)))
              (v (byte-eval m)) )
         (global-update! i v) ) )
      ((7) ; CONSTANT
       (let* ((value (vector-ref *byte-vector* (+ pc 1))))
         value ) )
      ((8) ; ALTERNATIVE
       (let* ((m1 (vector-ref *byte-vector* (+ pc 1)))
              (m2 (vector-ref *byte-vector* (+ pc 2)))
              (m3 (vector-ref *byte-vector* (+ pc 3)))
              (v (byte-eval m1)) )
         (byte-eval (if v m2 m3)) ) )
      ((9) ; SEQUENCE
       (let* ((m1 (vector-ref *byte-vector* (+ pc 1)))
              (m2 (vector-ref *byte-vector* (+ pc 2))) )
         (byte-eval m1)
         (byte-eval m2) ) )
      ((10) ; TR-FIX-LET
       (let* ((m* (vector-ref *byte-vector* (+ pc 1)))
              (m+ (vector-ref *byte-vector* (+ pc 2))) )
         (set! *env* (sr-extend* *env* (byte-eval m*)))
         (byte-eval m+) ) )
      ((11) ; FIX-LET
       (let* ((m* (vector-ref *byte-vector* (+ pc 1)))
              (m+ (vector-ref *byte-vector* (+ pc 2))) )
         (let ((sr *env*))
           (set! *env* (sr-extend* *env* (byte-eval m*)))
           (let ((result (byte-eval m+)))
             (set! *env* sr) 
             result ) ) ) )
      ((14) ; CALL0
       (let* ((address (vector-ref *byte-vector* (+ pc 1))))
         (address) ) )
      ((15) ; CALL1
       (let* ((address (vector-ref *byte-vector* (+ pc 1)))
              (m1 (vector-ref *byte-vector* (+ pc 2))) )
         (address (byte-eval m1)) ) )
      ((16) ; CALL2
       (let* ((address (vector-ref *byte-vector* (+ pc 1)))
              (m1 (vector-ref *byte-vector* (+ pc 2)))
              (m2 (vector-ref *byte-vector* (+ pc 3))) )
         (address (byte-eval m1) (byte-eval m2)) ) )
      ((17) ; CALL3
       (let* ((address (vector-ref *byte-vector* (+ pc 1)))
              (m1 (vector-ref *byte-vector* (+ pc 2)))
              (m2 (vector-ref *byte-vector* (+ pc 3)))
              (m3 (vector-ref *byte-vector* (+ pc 4))) )
         (address (byte-eval m1) (byte-eval m2) (byte-eval m3)) ) )
      ((18) ; FIX-CLOSURE
       (let* ((m+ (vector-ref *byte-vector* (+ pc 1)))
              (arity+1 (vector-ref *byte-vector* (+ pc 2))) )
         (create-fix-closure m+ arity+1) ) )
      ((19) ; NARY-CLOSURE
       (let* ((m+ (vector-ref *byte-vector* (+ pc 1)))
              (arity+1 (vector-ref *byte-vector* (+ pc 2))) )
         (create-nary-closure m+ arity+1) ) )
      ((20) ; TR-REGULAR-CALL
       (let* ((m (vector-ref *byte-vector* (+ pc 1)))
              (m* (vector-ref *byte-vector* (+ pc 2))) )
         (let ((f (byte-eval m)))
           (invoke f (byte-eval m*)) ) ) )
      ((21) ; REGULAR-CALL
       (let* ((m (vector-ref *byte-vector* (+ pc 1)))
              (m* (vector-ref *byte-vector* (+ pc 2))) )
         (let* ((f (byte-eval m))
                (v* (byte-eval m*))
                (sr *env*)
                (result (invoke f (byte-eval m*))) )
           (set! *env* sr)
           result ) ) )
      ((22) ; STORE-ARGUMENT
       (let* ((m (vector-ref *byte-vector* (+ pc 1)))
              (m* (vector-ref *byte-vector* (+ pc 2)))
              (rank (vector-ref *byte-vector* (+ pc 3))) )
         (let* ((v (byte-eval m))
                (v* (byte-eval m*)) )
           (set-activation-frame-argument! v* rank v)
           v* ) ) )
      ((23) ; ALLOCATE-FRAME
       (let* ((size+1 (vector-ref *byte-vector* (+ pc 1))))
         (allocate-activation-frame size+1) ) )
      ((24) ; CONS-ARGUMENT
       (let* ((m (vector-ref *byte-vector* (+ pc 1)))
              (m* (vector-ref *byte-vector* (+ pc 2)))
              (arity (vector-ref *byte-vector* (+ pc 3))) )
         (let* ((v (byte-eval m))
                (v* (byte-eval m*)) )
           (set-activation-frame-argument! 
            v* arity (cons v (activation-frame-argument v* arity)) )
           v* ) ) )
      ((25) ; ALLOCATE-DOTTED-FRAME
       (let* ((arity (vector-ref *byte-vector* (+ pc 1)))
              (v* (allocate-activation-frame (+ arity 1))) )
         (set-activation-frame-argument! v* arity '())
         v* ) )
      (else 
       (wrong "Unknown byte-code" byte) ) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooNEW
;;; Testing

(define (scheme6e)
  (interpreter 
   "Scheme? "  
   "Scheme= " 
   #t
   (lambda (read print error)
     (set! wrong error)
     (set! static-wrong error)
     (lambda ()
       (let ((ep (byte-compile (read))))
         (set! *env* r.init)
         (print (byte-eval ep)) ) ) ) ) )

(define (test-scheme6e file)
  (suite-test 
   file 
   "Scheme? " 
   "Scheme= "
   #t
   (lambda (read check error)
     (set! wrong error)
     (set! static-wrong error)
     (lambda ()
       (let* ((ep (byte-compile (read)))
              (size *byte-index*) )
         (format #t ";;; Code size: ~A,	Entry Point: ~A.~%"
                 size ep )
         (set! *env* r.init)
         (let ((v (byte-eval ep)))
           (unless (= size *byte-index*)
             (wrong "Code changed!!!" *byte-vector*) )
           (check v) ) )) )
   equal? ) )

;;; Pay attention to tail-rec in Scheme->C.

(define (bench6e factor e)
  (let ((start (get-internal-run-time))
        (ep (byte-compile e)) )             
    (let loop ((factor factor))
      (set! *env* r.init)
      (let ((v (byte-eval ep)))
        (let ((duration (- (get-internal-run-time) start)))
          (when (<= factor 1)
            (display (list duration v))
            (newline) ) ) )
      (if (> factor 1)
          (loop (- factor 1)) ) ) ) )

;;; end of chap6e.scm
