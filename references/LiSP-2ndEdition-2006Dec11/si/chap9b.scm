;;; $Id: chap9b.scm,v 1.2 1994/08/21 19:35:32 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

(define (fact1 n) (if (= n 0) 1 (* n (fact1 (- n 1)))))

(define-abbreviation (factorial n)
  (define (fact2 n) (if (= n 1) 1 (* n (fact2 (- n 1)))))
  (if (and (integer? n) (> n 0)) (fact2 n) `(fact1 ,n)) )

(define (some-facts)
  (list (factorial 5) (factorial (+ 3 2))) )

;;; end of chap9b.scm
