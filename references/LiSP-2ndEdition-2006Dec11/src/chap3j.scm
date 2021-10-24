;;; $Id$

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

(define (evaluate-begin e* r k)
  (if (pair? e*)
      (evaluate (car e*) r (make-begin-cont k e* r))
      (resume k empty-begin-value) ) )

(define-method (resume (k begin-cont) v)
  (let ((e* (cdr (begin-cont-e* k))))
   (if (pair? e*)
       (evaluate-begin e* (begin-cont-r k) (begin-cont-k k))
       (resume (begin-cont-k k) v) ) ) )

(define-class no-more-argument-cont continuation ())

(define (evaluate-arguments e* r k)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (evaluate (car e*) r (make-argument-cont k e* r))
          (evaluate (car e*) r (make-no-more-argument-cont k)) )
      (resume k no-more-arguments) ) )

(define-method (resume (k no-more-argument-cont) v) 
  (resume (no-more-argument-cont-k k) (list v)) )

(definitial throw
   (make-primitive
    'throw
    (lambda (v* r k)
      (if (= 2 (length v*))
          (catch-lookup k (car v*) 
                        (make-throw-cont k `(quote ,(cadr v*)) r) )
          (wrong "Incorrect arity" 'throw v*) ) ) ) )

(definitial funcall)

(define-class reified-continuation value (k))

(definitial call/cc
  (make-primitive 'call/cc
                  (lambda (v* r k)
                    (if (= 1 (length v*))
                        (invoke (car v*) 
                                (list (make-reified-continuation k))
                                r
                                k )
                        (wrong "Incorrect arity" 'call/cc v*) ) ) ) )

(define-method (invoke (f reified-continuation) v* r k)
  (if (= 1 (length v*))
      (resume (reified-continuation-k f) (car v*))
      (wrong "Continuations expect one argument" v* r k) ) )

(define-class function-with-arity function (arity))

(define (evaluate-lambda n* e* r k)
  (resume k (make-function-with-arity n* e* r (length n*))) )

(define-method (invoke (f function-with-arity) v* r k)
  (if (= (function-with-arity-arity f) (length v*))
      (let ((env (extend-env (function-env f)
                             (function-variables f)
                             v* )))
         (evaluate-begin (function-body f) env k) )
      (wrong "Incorrect arity" (function-variables f) v*) ) ) 

(define-class function-nadic function (arity))

(define (evaluate-lambda n* e* r k)
  (define (length n*)
    (if (pair? n*) (+ 1 (length (cdr n*))) 0) )
  (resume k (if (null? (cdr (last-pair (cons 'n n*))))
                (make-function-with-arity n* e* r (length n*))
                (make-function-nadic n* e* r (length n*)) )) )

(define-method (invoke (f function-nadic) v* r k)
  (define (extend-env env names values)
    (if (pair? names)
         (make-variable-env  
          (extend-env env (cdr names) (cdr values))
          (car names)
          (car values) )
        (make-variable-env env names values) ) )
  (if (>= (length v*) (function-nadic-arity f))
      (let ((env (extend-env (function-env f)
                             (function-variables f)
                             v* )))
         (evaluate-begin (function-body f) env k) )
      (wrong "Incorrect arity" (function-variables f) v*) ) )

(define (chap3j-interpreter)
  (letrec ((k.init (make-bottom-cont 
                    'void (lambda (v) (display v)
                                      (toplevel) ) ))
           (toplevel (lambda () (evaluate (read) r.init k.init))) )
     (toplevel) ) )


;;; end of chap3j.scm
