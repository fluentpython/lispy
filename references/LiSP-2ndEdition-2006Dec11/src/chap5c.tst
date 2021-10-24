;;; Testing dynamic binding

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

(dynamic-let (a 1) 2)
   2
(dynamic-let (a 1) 2 3 4)
   4
(dynamic-let (a 1) (dynamic a))
   1
(dynamic-let (a 1) (+ (dynamic a) (dynamic a)))
   2
(dynamic-let (a 1)
  (dynamic-let (a (+ (dynamic a) (dynamic a)))
     (dynamic a) ) )
   2

;;; with respect to functions
((lambda (f)
   (dynamic-let (a 1) (f)) ) 
 (lambda () (dynamic a)) )
   1
((lambda (f)
   (dynamic-let (a 1)
     (list (f)
           (dynamic-let (a 2) (f)) ) ) )
 (lambda () (dynamic a)) )
   (1 2)

;;; with respect to continuations
(dynamic-let (a 1)
   ((call/cc (lambda (k)
               ((lambda (f) 
                   (dynamic-let (a 2) f) )                      
                (lambda () (dynamic a)) ) ))) )
   1
