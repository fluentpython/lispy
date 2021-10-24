;;; $Id: chap10ex.scm,v 4.0 1995/07/10 06:50:39 queinnec Exp $ 

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; This is the running example of chapter 10. This expression will be
;;; compiled to C in the src/c/chap10ex.c file. It contains at least
;;; one example of all syntactic structures.

(begin
  (set! index 1)
  ((lambda (cnter . tmp)
      (set! tmp (cnter (lambda (i) (lambda x (cons i x)))))
      (if cnter (cnter tmp) index) )
   (lambda (f) 
     (set! index (+ 1 index))
     (f index) )
   'foo ) ) 

;;; end of chap10ex.scm
