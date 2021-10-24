;;; $Id: chap2h.tst,v 4.0 1995/07/10 06:51:08 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

(2 '(a b c d))
   c
(-1 '(a b c))
   (b c)

(2 '(foo bar hux wok))  
   hux
(-2 '(foo bar hux wok)) 
   (hux wok)
(0 '(foo bar hux wok))  
  foo
(2 (-3 '(a b c d e f g h)))
   f

((list + - *) 5 3)
   (8 2 15)

;;; end of chap2h.tst
