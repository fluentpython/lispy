;;; $Id: chap5g.tst,v 4.0 1995/07/10 06:51:32 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

(define zlurp (+ 2 3))
   ---
zlurp
   5
(set! zlurp 6)
   ---
zlurp
   6

((lambda (zixxy)
   (define zixxy (* 2 2)) )
 3 )
   ---
zixxy 
   4

;;; end of chap5g.tst
