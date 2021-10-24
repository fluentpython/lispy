;;; Some tests for chap5e.scm

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

3
   (3)
'(a b)         
   ((a b))
(car '(a b))    
   (a a)
(if 1 2 3)     
   (2)
(+ 2 3)
   (5 5 5 5 5 5)
(list 2 3 4)
   ((2 3 4) (2 3 4) (2 3 4) (2 3 4) (2 3 4) (2 3 4)
    (2 3 4) (2 3 4) (2 3 4) (2 3 4) (2 3 4) (2 3 4)
    (2 3 4) (2 3 4) (2 3 4) (2 3 4) (2 3 4) (2 3 4)
    (2 3 4) (2 3 4) (2 3 4) (2 3 4) (2 3 4) (2 3 4) )
(call/cc 
 (lambda (k) 
   ((k 1) (k 2)) ) )
    (1 1 1 1 2 2 2 2)

;;; end of chap5e.tst
