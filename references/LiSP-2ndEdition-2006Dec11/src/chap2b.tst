;;; $Id: chap2b.tst,v 4.0 1995/07/10 06:50:58 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; testing chap2b

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


(flet ((square (x) (* x x)))
      (square 3) )
   9
(apply (function cons) 1 2 '())
   (1 . 2)
(funcall (function cons) 1 2)
   (1 . 2)
(funcall (function funcall) (function cons) 1 2)
   (1 . 2)

((lambda (f)(apply f (list 3)))
 (flet ((square (x) (* x x)))
       (function (lambda (x) (square (square x)))) ))
    81

(labels ((fact (n) (if (= n 0) 1 (* n (fact (- n 1))))))
  (fact 5) )
   120
(labels ((odd? (n) (if (= n 0) #f (even? (- n 1))))
         (even? (n) (if (= n 0) #t (odd? (- n 1)))) )
   (list (odd? 5) (odd? 4) (even? 5) (even? 4)) )
   (#t #f #f #t)

;;; end of chap2b.tst
