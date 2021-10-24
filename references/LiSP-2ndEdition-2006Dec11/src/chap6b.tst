;;; $Id: chap6b.tst,v 4.0 1995/07/10 06:51:36 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; testing that new variables are automatically created.

pqoeur
  ***
(begin (set! zld 3)
        zld )
  3
(lambda () oiuerq 3)
   ---
((lambda (x) oeewfiuerq x) 3)
   ***

;;; end of chap6b.tst
