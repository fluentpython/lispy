;;; $Id$

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; This file introduces a lot of things to test.
;;; (compile-file "si/foo")
;;; (run-application 100 "si/foo")

(set! foo '(a b))

(set! bar 
      (lambda (x) 
        (cons x (dynamic x)) ) )

(set! hux (dynamic-let (x '(c d))
            (dynamic-let (y 'foo)
              (dynamic-let (x foo)
                (bar 'bar) ) ) ))

(display (list foo hux))

;;; end of foo.scm
