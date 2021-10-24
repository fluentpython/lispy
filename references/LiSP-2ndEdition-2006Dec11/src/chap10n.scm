;;; $Id: chap10n.scm,v 4.0 1995/07/10 06:50:50 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Tests: this function redefines the one of chap10f.scm (for use by
;;; chap10e.scm) and allows to test letify in conjunction with chap10e.

(define (compile->C e out)
  (set! g.current '())
  (let* ((ee (letify (Sexp->object e) '()))
         (prg (extract-things! (lift! ee))) )
    (gather-temporaries! (closurize-main! prg))
    (generate-C-program out e prg) ) )

;;; end of chap10n.scm
