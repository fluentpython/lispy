;;; $Id: chap1b.scm,v 4.0 1995/07/10 06:50:53 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;;                     variants of chapter 1.
;;; Sort of dynamic binding implementation.

;;; The version that will be included in the book

(define (d.evaluate e env)
  (if (atom? e) \ldots
      (case (car e)
        \ldots
        ((lambda) (d.make-function (cadr e) (cddr e) env))
        (else (d.invoke (d.evaluate (car e) env)
                        (evlis (cdr e) env)
                        env )) ) ) )           ; current environment

;;; The complete one.

(define (d.evaluate e env)
  (if (atom? e) 
      (cond ((symbol? e) (lookup e env))
            ((or (number? e)(string? e)(char? e)(boolean? e)(vector? e))
             e )
            (else (wrong "Cannot evaluate" e)) )
      (case (car e)
        ((quote)  (cadr e))
        ((if)     (if (not (eq? (d.evaluate (cadr e) env) the-false-value))
                      (d.evaluate (caddr e) env)
                      (d.evaluate (cadddr e) env) ))
        ((begin)  (eprogn (cdr e) env))
        ((set!)   (update! (cadr e) env (d.evaluate (caddr e) env)))
        ((lambda) (d.make-function (cadr e) (cddr e) env))
        (else (d.invoke (d.evaluate (car e) env)
                        (evlis (cdr e) env)
                        env )) ) ) )           ; current environment

(define (d.invoke fn args env)
  (if (procedure? fn) 
      (fn args env)
      (wrong "Not a function" fn) ) )

(define (d.make-function variables body env)
  (lambda (values current.env)
     (eprogn body (extend current.env variables values)) ) )

(define (d.make-closure fun env)
  (lambda (values current.env)
     (fun values env) ) )

;;; end of chap1b.scm
