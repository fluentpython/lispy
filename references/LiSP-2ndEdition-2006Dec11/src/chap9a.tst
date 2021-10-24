;;; $Id: chap9a.tst,v 4.0 1995/07/10 06:52:15 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; testing chap9a.scm
;;; If define-abbreviation is not defined, here is a simulation
(define-pervasive-macro (define-abbreviation . parms)
  `(define-pervasive-macro . ,parms) )
   ---

(with-gensym (a b c) 
  (list a b c) )
   ---

;;; exercice 1
(let ((i 0)
      (r '()) )
  (repeat1 :while (begin (set! i (+ i 1)) (< i 10))
           :unless (= 0 (modulo i 2))
           :do (set! r (cons i r)) )
  r  )
  (9 7 5 3 1)

(pp (expand '(generate-vector-of-fix-makers 3)))
   ---

(define-alias gg generate-vector-of-fix-makers)
   ---
(pp (expand '(gg 3)))
   ---

;;; tests chap9b.tst
(enumerate)
   --- ; prints 0
(enumerate (display 'a))
   --- ; prints 0a1
(enumerate (display 'a) (display 'b))
   --- ; prints 0a1b2
(enumerate (display 'a) (display 'b) (display 'c))
   --- ; prints 0a1b2c3
(enumerate (display 'a) (display 'b) (display 'c) (display 'd))
   --- ; prints 0a1b2c3d4

(meroon-if #t 1 2)
   1
(meroon-if #f 1 2)
   2
(meroon-if '() 1 2)
   1

(define-inline (acons x y al)
  (cons (cons x y) al) )
   ---
(acons 'a 1 (acons 'b 2 '()))
   ((a . 1) (b . 2))
(apply acons 'a '1 '(()))
   ((a . 1))

;;; end of chap9a.tst
