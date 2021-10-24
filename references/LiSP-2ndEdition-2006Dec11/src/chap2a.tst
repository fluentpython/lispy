;;; $Id: chap2a.tst,v 4.0 1995/07/10 06:50:56 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Testing the chap2a interpreter

33
   33
xyy 
   *** ; unexistant
'foo
   foo
(if #t 1 2)
   1
(if #f 2 3)
   3
(begin 1 2 3)
   3
(begin (set! a 3) a)
   3
(cons 3 4)
   (3 . 4)
((lambda (x y) (cons x y))
 1 2 )
   (1 . 2)
cons 
   *** ; cons not a variable
((lambda (f) (f 1 2))
 cons )
   *** ; cons not a variable
(apply (lambda (x y) (cons y x)) '1 '2 '())
   (2 . 1)

; no computation in functional position
((if #t cons list) 1 22)
   ***


;;; end of chap2a.tst
