;;; $Id: chap5h.scm,v 4.0 1995/07/10 06:51:33 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Simulate an unprescribed evaluation order. This is weaker than the
;;; denotation since the order of the evaluation of all arguments is
;;; chosen at the beginning.

(define-syntax unordered
  (syntax-rules ()
    ((unordered f) (f))
    ((unordered f arg ...)
     (determine! (lambda () f) (lambda () arg) ... ) ) ) )

;;; NOTE: due to multiple return of continuation, don't replace
;;; the-thunk by (car thunk-ref). This is false if the corresponding
;;; continuation is invoked more than once.

(define (determine! . thunks)
  (let ((results (iota 0 (length thunks))))
    (let loop ((permut (random-permutation (length thunks))))
      (if (pair? permut)
          (begin (set-car! (list-tail results (car permut))
                           (force (list-ref thunks (car permut))) )
                 (loop (cdr permut)) )
          (apply (car results) (cdr results)) ) ) ) )

(define (random-permutation n)
  (shuffle (iota 0 n)) )

(define (d.determine! . thunks)
  (let ((results (iota 0 (length thunks))))
    (let loop ((permut (shuffle (iota 0 (length thunks)))))
      (if (pair? permut)
          (begin (set-car! (list-tail results (car permut))
                           (force (list-ref thunks (car permut))) )
                 (loop (shuffle (cdr permut))) )
          (apply (car results) (cdr results)) ) ) ) )

;;; Retrofit
(set! determine! d.determine!)

;;; Patch combination meaning to use unordered. For that, use a simple
;;; interpreter (chap1, the naive one) and insert unordered somewhere
;;; in it.

(define (evlis exps env)
  (case (length exps)
    ((0) '())
    ((1) (unordered list (evaluate (car exps) env)))
    ((2) (unordered list 
                    (evaluate (car exps) env)
                    (evaluate (cadr exps) env) ))
    ((3) (unordered list 
                    (evaluate (car exps) env)
                    (evaluate (cadr exps) env)
                    (evaluate (caddr exps) env) ))
    ((4) (unordered list 
                    (evaluate (car exps) env)
                    (evaluate (cadr exps) env)
                    (evaluate (caddr exps) env)
                    (evaluate (cadddr exps) env) ))
    (else (unordered cons 
                     (evaluate (car exps) env)
                     (evlis (cdr exps) env) )) ) )
;;; Test: (test-scheme1 "src/scheme.tst")

(define (linear-random-maker m a seed)
  (lambda (n)
    (set! seed (modulo (* a seed) m))
    (modulo seed n) ) )
(define rand (linear-random-maker (* 1024 16384) 51 1))

(define (test max)
  (do ((i 0 (+ 1 i))
       (sum 0) )
      ((= i max) (/ sum i))
    (let ((n (rand max)))
      (display n)(display "  ")
      (set! sum (+ sum n)) ) ) )

(define (shuffle r)
  (do ((i n (- i 1)))
      ((= i 0) r)
    (let scan ((index (rand n))
               (left '())
               (right r))
      (if (> index 0)
          (scan (- index 1)
                (cons (car right) left)
                (cdr right) )
          (set! r (append right left)) ) ) ) )
;;; Test:
;;; (random-permutation 5)

;;; end of chap5h.scm
