;;; $Id: chap8b.scm,v 4.3 2006/11/27 11:34:27 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Modification of chap4a.scm to introduce eval as a special form

(define (evaluate e r s k)
  (if (atom? e)
      (if (symbol? e) (evaluate-variable e r s k)
          (evaluate-quote e r s k) )
      (case (car e)
        ((quote)  (evaluate-quote (cadr e) r s k))
        ((if)     (evaluate-if (cadr e) (caddr e) (cadddr e) r s k))
        ((begin)  (evaluate-begin (cdr e) r s k))
        ((set!)   (evaluate-set! (cadr e) (caddr e) r s k))
        ((lambda) (evaluate-lambda (cadr e) (cddr e) r s k))
        ((eval)   (evaluate-eval (cadr e) r s k))       ; \modified
        (else     (evaluate-application (car e) (cdr e) r s k)) ) ) )

(define (evaluate-eval e r s k)
  (evaluate e r s
    (lambda (v ss)
      (let ((ee (transcode-back v ss)))
        (if (program? ee)
            (evaluate ee r ss k)
            (wrong "Illegal program" ee) ) ) ) ) )

;;; Create some additional locations

(definitial bar 0)
(definitial x 0)
(definitial z 0)
(definitial wrek 0)

(defprimitive display
  (lambda (v* s k)
    (display (transcode-back (car v*) s))
    (k (car v*) s) )
  1 )

;;; end of chap8b.scm
