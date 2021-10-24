;;; $Id$

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Test for chap7e.scm.
;;; Be above directories /si/ and /so/ then evaluate:
;;;     (compile-file "si/fact")
;;; It generates the file si/fact.so that can be read.

(set! fact
      (lambda (n)
        (if (= n 0) 1
            (* n (fact (- n 1))) ) ) )

;;;; end of fact.scm
