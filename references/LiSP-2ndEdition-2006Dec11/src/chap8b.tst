;;; $Id: chap8b.tst,v 4.0 1995/07/10 06:52:02 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Eval with dynamic variables

"Testing dynamic variables"
   ---
(dynamic-let (a 1)
  (eval '(dynamic a)) )
   1
(dynamic-let (a 1)
  (dynamic-let (b 2)
    (eval '(dynamic b)) ) )
   2
((lambda (f)
   (dynamic-let (a 33)
     ((eval 'f)) ) )
 (lambda () (dynamic a)) )
   33

;;; end of chap8b.tst
