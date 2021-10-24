;;; $Id: chap3a.tst,v 4.0 1995/07/10 06:51:09 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Test for chap3a.scm

(find-symbol 'a '(a))
   #t
(find-symbol 'a '(b  . c))
   #f
(find-symbol 'a '((((b . c) . d) e f . g) . a))
   #t
(find-symbol 'a '((((b . c) . d) e f . g) . h))
   #f

;;; end of chap3a.tst
