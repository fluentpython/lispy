;;; $Id: chap8j.scm,v 4.1 1996/01/14 14:14:29 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; A reflective interpreter with non-systematically reified
;;; continuation and environment. These can be obtained through export
;;; and call/cc. Special forms are coded as fexprs.

(define (meaning e r tail?)
  (if (atom? e)
      (if (symbol? e) (meaning-reference e r tail?)
                      (meaning-quotation e r tail?) )
      (case (car e)
        ((quote)  (meaning-quotation (cadr e) r tail?))
        ((lambda) (meaning-abstraction (cadr e) (cddr e) r tail?))
        ((if)     (meaning-alternative (cadr e) (caddr e) (cadddr e) r tail?))
        ((begin)  (meaning-sequence (cdr e) r tail?))
        ((set!)   (meaning-assignment (cadr e) (caddr e) r tail?))
        ((bind-exit) (meaning-bind-exit (caadr e) (cddr e) r tail?))
        ((dynamic) (meaning-dynamic-reference (cadr e) r tail?))
        ((dynamic-let) (meaning-dynamic-let (car (cadr e))
                                            (cadr (cadr e))
                                            (cddr e) r tail? ))
        ((monitor) (meaning-monitor (cadr e) (cddr e) r tail?))
        ((the-environment)  (meaning-export '() r tail?))                ; \modified
        (else      (meaning-application (car e) (cdr e) r tail?)) ) ) )

;;; redefine these methods to hide details

(define-method  (show (e environment) . stream)
  (call-next-method) )

(define-method  (show (e activation-frame) . stream)
  (call-next-method) )

;;; (compile-file "si/reflisp")			
;;; (run-application 400 "si/reflisp.so")

;;; To know the length of the interpreter: (vector-length *code*)
;;; Actually 1270 bytes (with procedure->definition and others)

;;; end of chap8j.scm
