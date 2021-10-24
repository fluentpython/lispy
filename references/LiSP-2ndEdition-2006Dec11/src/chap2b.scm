;;; $Id: chap2b.scm,v 4.1 2006/11/09 18:55:33 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Variants arount chapter 2 (uses chap2a.scm)
;;; Addition of flet, labels, function, funcall

(define (f.evaluate e env fenv)
  (if (atom? e) 
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e)
                 (boolean? e) (vector? e) )
             e )
            (else (wrong "Cannot evaluate" e)) )
      (case (car e)
        ((quote)  (cadr e))
        ((if)     (if (f.evaluate (cadr e) env fenv)
                      (f.evaluate (caddr e) env fenv)
                      (f.evaluate (cadddr e) env fenv) ))
        ((begin)  (f.eprogn (cdr e) env fenv))
        ((set!)   (update! (cadr e) 
                           env 
                           (f.evaluate (caddr e) env fenv) ))
        ((lambda) (f.make-function (cadr e) (cddr e) env fenv))
        ((function)
         (cond ((symbol? (cadr e))
                (f.lookup (cadr e) fenv) )
               ((and (pair? (cadr e)) (eq? (car (cadr e)) 'lambda))
                (f.make-function 
                 (cadr (cadr e)) (cddr (cadr e)) env fenv ) )
               (else (wrong "Incorrect function" (cadr e))) ) )
        ((flet)
         (f.eprogn (cddr e)
                   env
                   (extend fenv
                           (map car (cadr e))
                           (map (lambda (def)
                                  (f.make-function (cadr def) 
                                                   (cddr def)
                                                   env fenv ) )
                                (cadr e) ) ) ) )
        ((labels)
         (let ((new-fenv (extend fenv
                                 (map car (cadr e))
                                 (map (lambda (def) 'void) 
                                      (cadr e) ) )))
           (for-each (lambda (def)
                       (update! (car def)
                                new-fenv
                                (f.make-function (cadr def) 
                                                 (cddr def) 
                                                 env new-fenv ) ) )
                     (cadr e) )
           (f.eprogn (cddr e) env new-fenv ) ) )
        (else     (f.evaluate-application (car e) 
                                          (f.evlis (cdr e) env fenv)
                                          env 
                                          fenv )) ) ) )

(define (f.evaluate-application fn args env fenv)
  (cond ((symbol? fn)
         ((f.lookup fn fenv) args) )
        ((and (pair? fn) (eq? (car fn) 'lambda))
         (f.eprogn (cddr fn)
                   (extend env (cadr fn) args)
                   fenv ) )
        (else (wrong "Incorrect functional term" fn)) ) )

(define (f.lookup id fenv)
  (if (pair? fenv)
      (if (eq? (caar fenv) id)
          (cdar fenv)
          (f.lookup id (cdr fenv)) )
      (lambda (values)
        (wrong "No such functional binding" id) ) ) )  

(definitial-function funcall
  (lambda (args)
    (if (> (length args) 1)
        (invoke (car args) (cdr args))
        (wrong "Incorrect arity" 'funcall) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;;  Tests

(set! *tests-to-skip* '())

;;; end of chap2b.scm
