;;; $Id: chap10j.tst,v 4.0 1995/07/10 06:50:46 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Some tests about initialization analysis

(set! x 22)
   22
(set! x y)
   *** ; y uninitialized
(begin (set! x (lambda () y))
       (set! y (lambda () x))
       (x) )
   ---
(begin (set! x (lambda () y))
       (set! y (x)) )
   *** ; y uninitialized
((lambda (x) 
   (set! y x)
   y )
 33 )
   33

;;; end of chap10j.tst
