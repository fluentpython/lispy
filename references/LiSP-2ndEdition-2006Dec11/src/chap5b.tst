;;; Test for Lambda-calculus

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; The initial environment maps identifiers to numbers and also contain
;;; the addition as a delta rule.

a
   1
b
   2
(+ a)
   ---
((+ a) b)
   3

((lambda (K) ((K a) b))
 (lambda (x) (lambda (y) x)) )
   1

((lambda (S) 
   ((lambda (K) (((S K) K) b))
    (lambda (x) (lambda (y) x)) ) )
 (lambda (f) (lambda (g) (lambda (x) ((f x) (g x))))) )
   2

((label fact (lambda (x)
               ((((= x) a)
                 a )
                ((* x) (fact ((- x) a))) ) ))
  a )
    1
((label fact (lambda (x)
               ((((= x) a)
                 a )
                ((* x) (fact ((- x) a))) ) ))
  ((+ b) b) )
    24

;;; this one loops 
((lambda (Y)
   ((lambda (meta-fact)
      ((Y meta-fact) ((+ b) b)) )
    (lambda (f)
      (lambda (x)
        ((((= x) a)
          a )
         ((* x) (f ((- x) a))) ) ) ) ) )
 (lambda (f)
   ((lambda (x) (f (lambda (y) ((x x) y))))
    (lambda (x) (f (lambda (y) ((x x) y)))) ) ) )
   24

;;; end of chap5b.tst
