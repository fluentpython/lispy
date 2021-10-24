;;; $Id: chap8c.scm,v 4.3 2006/11/24 18:41:11 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Modification of chap6d.scm to introduce eval as a special form

(define (meaning e r tail?)
  (if (atom? e)
      (if (symbol? e) (meaning-reference e r tail?)
                      (meaning-quotation e r tail?) )
      (case (car e)
        ((quote)  (meaning-quotation (cadr e) r tail?))
        ((lambda) (meaning-abstraction (cadr e) (cddr e) r tail?))
        ((if)     (meaning-alternative (cadr e) (caddr e) (cadddr e) 
                                       r tail? ))
        ((begin)  (meaning-sequence (cdr e) r tail?))
        ((set!)   (meaning-assignment (cadr e) (caddr e) r tail?))
        ((eval)   (meaning-eval (cadr e) r tail?))        ; \modified
        (else     (meaning-application (car e) (cdr e) 
                                       r tail? )) ) ) )

(define (meaning-eval e r tail?)
  (let ((m (meaning e r #f)))
    (lambda ()
      (let ((v (m)))
        (if (program? v)
            (let ((mm (meaning v r tail?)))
              (mm) )
            (wrong "Illegal program" v) ) ) ) ) )

;;; Add additional locations

(defvariable wrek)
(define original.g.current
  (let ((g g.current))
    (lambda () g) ) )

;;; end of chap8c.scm
