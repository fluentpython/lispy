;;; Particular tests for the interpreter

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; testing eq?
(eq? 'a 'b)
   #f
(eq? 'a 'a)
   #t
(eq? (cons 1 2) (cons 1 2))
   #f
((lambda (a) (eq? a a))
 (cons 1 2) )
   #t
((lambda (a) (eq? a a))
 (lambda (x) x) )
   #t
(eq? (lambda (x) 1) (lambda (x y) 2))
   #f

;;; testing eqv? (same as eq? plus eqv?)
(eqv? '1 '2)
   #f
(eqv? 1 1)
   #t
(eqv? 'a 'b)
   #f
(eqv? 'a 'a)
   #t
(eqv? (cons 1 2) (cons 1 2))
   #f
((lambda (a) (eqv? a a))
 (cons 1 2) )
   #t
((lambda (a) (eqv? a a))
 (lambda (x) x) )
   #t
(eqv? (lambda (x) 1) (lambda (x y) 2))
   #f

;;; Testing the special OR (backtracking without side-effect).
((lambda (x)
   (or (begin (set! x (+ x 1))
              #f )
       (if (= x 1) 'OK 'KO) ) )
 1 )
   OK
((lambda (x)
   (or (begin (set! x (+ x 1))
              #f )
       (if (= x 1) (begin (set! x 3) x) 'KO) ) )
 1 )
   3
