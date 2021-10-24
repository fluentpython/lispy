;;; $Id: chap6dd.tst,v 4.0 1995/07/10 06:51:40 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Tests for chap6dd.scm

(redefine car)
   ---
(set! car car)
   ---
(car '(a b))
   a
((lambda (old v)
   (begin (set! old car)
          (set! car cdr)
          (set! v (car '(a b)))
          (set! car old)
          v ) )
  'old 'v )
   (b)

;;; testing errors
(redefine car)
  ***
(redefine foo)
  ***

;;; end of chap6dd.tst
