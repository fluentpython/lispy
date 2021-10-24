;;; $Id: chap5b.scm,v 4.0 1995/07/10 06:51:23 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; Semantics of the Lambda calculus

(define (L-meaning e)
  ;(format #t "Meaning of ~a.~%" e)
  (cond ((symbol? e) (L-meaning-reference e))
        ((eq? (car e) 'lambda)
                     (L-meaning-abstraction (car (cadr e)) (caddr e)) )
        ((eq? (car e) 'label)
         (L-meaning-label (cadr e) (caddr e)) )
        (else        (L-meaning-combination (car e) (cadr e))) ) )

(define ((L-meaning-reference n) r)
  (r n) )

(define ((L-meaning-abstraction n e) r)
  (lambda (v) 
    ((L-meaning e) (extend r n v)) ) )

(define ((L-meaning-combination e1 e2) r)
  (((L-meaning e1) r) ((L-meaning e2) r)) )

(define ((L-meaning-label n e) r)
  (fix (lambda (v)
         ((L-meaning e) (extend r n v)) )) )

(define fix
  (let ((d (lambda (w)
             (lambda (f)
               (f (lambda (x) (((w w) f) x))) ) )))
    (d d) ) )


;;; Initial environment

(define (r.init n)
  (wrong "No such variable" n) )

(define (extend fn pt im)
  (lambda (x) (if (equal? pt x) im (fn x))) )

(set! r.init (extend r.init 'a 1))
(set! r.init (extend r.init 'b 2))
(set! r.init (extend r.init 'c 3))
(set! r.init (extend r.init '+ (lambda (x)
                                 (lambda (y) (+ x y)) )))
(set! r.init (extend r.init '- (lambda (x)
                                 (lambda (y) (- x y)) )))
(set! r.init (extend r.init '* (lambda (x)
                                 (lambda (y) (* x y)) )))
(set! r.init (extend r.init '= (lambda (x)
                                 (lambda (y) 
                                   (if (= x y)
                                       (lambda (x) (lambda (y) x))
                                       (lambda (x) (lambda (y) y)) ) ) ) ) )

;;; Testing 

(define (test-L file)
  (suite-test
   file
   "lambda? "
   "lambda= "
   #t
   (lambda (read check error)
     (set! wrong error)
     (lambda () 
       (check ((L-meaning (read))
               r.init ) ) ) )
   equal? ) )

;;; end of chap5b.scm
