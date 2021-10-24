;;; $Id: chap2g.tst,v 4.0 1995/07/10 06:51:06 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; tests on let.

(let () 33)
   33
(let ((x 1)) x)
   1
(let ((x (* 2 3))
      (y (* 4 5)) )
  (+ x y) )
   26
(let ((x 1))
  1 2 3 )
   3

;;; tests on uninitialied variables
xyzy
   *** ; undefined

;;; tests on letrec
(letrec ((fact (lambda (n)
                 (if (= n 1) 1 (* n (fact (- n 1)))) )))
   (letrec ((odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
            (even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) )
     (list (fact 5) (odd? 5) (odd? 4) (even? 4) (even? 3)) ) )
   (120 #t #f #t #f)

((label fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1))))))
 5 )
   120

;;; end of chap2g.tst
