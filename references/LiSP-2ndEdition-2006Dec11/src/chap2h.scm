;;; $Id: chap2h.scm,v 4.0 1995/07/10 06:51:07 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Add innovation to chap1.scm ie allow (2 '(a b c)) or ((car cdr) '(a b)).

(define (invoke fn args)
  (cond ((procedure? fn) (fn args))
        ((number? fn)
         (if (= (length args) 1)
             (if (>= fn 0) (list-ref (car args) fn)
                 (list-tail (car args) (- fn)) )
             (wrong "Incorrect arity" fn) ) )
        ((pair? fn)
         (map (lambda (f) (invoke f args))
              fn ) )
        (else (wrong "Cannot apply" fn)) ) )

;;; end of chap2h.scm
