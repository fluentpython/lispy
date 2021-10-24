;;; $Id: after.scm,v 1.1 1994/09/03 16:18:20 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; This file uses some global variables. It tests global variable
;;; linking. Compile it with :
;;;     (compile-file "si/after")
;;; builds the executable with:
;;;     (build-application "si/a.out" "si/fact" "si/fib" "si/foo" "si/after")
;;; and runs the total program with:
;;;     (run-application 400 "si/a.out")

(set! fib fact)

(display (list (fact 6)  ; 89
               (dynamic-let (x -34)
                            (bar 'bar) )
               ;; force a long-goto
               (if (fib 1)
                   (list 1 2 3 4 5 6 7 8 9 10
                         1 2 3 4 5 6 7 8 9 20
                         1 2 3 4 5 6 7 8 9 30
                         1 2 3 4 5 6 7 8 9 40
                         1 2 3 4 5 6 7 8 9 50
                         1 2 3 4 5 6 7 8 9 60
                         1 2 3 4 5 6 7 8 9 70
                         1 2 3 4 5 6 7 8 9 80
                         1 2 3 4 5 6 7 8 9 90
                         1 2 3 4 5 6 7 8 9 100
                         1 2 3 4 5 6 7 8 9 110
                         1 2 3 4 5 6 7 8 9 120
                         1 2 3 4 5 6 7 8 9 130
                         1 2 3 4 5 6 7 8 9 140
                         1 2 3 4 5 6 7 8 9 150 #t)
                   (list 1 2 3 4 5 6 7 8 9 10
                         1 2 3 4 5 6 7 8 9 20
                         1 2 3 4 5 6 7 8 9 30
                         1 2 3 4 5 6 7 8 9 40
                         1 2 3 4 5 6 7 8 9 50
                         1 2 3 4 5 6 7 8 9 60
                         1 2 3 4 5 6 7 8 9 70
                         1 2 3 4 5 6 7 8 9 80
                         1 2 3 4 5 6 7 8 9 90
                         1 2 3 4 5 6 7 8 9 100
                         1 2 3 4 5 6 7 8 9 110
                         1 2 3 4 5 6 7 8 9 120
                         1 2 3 4 5 6 7 8 9 130
                         1 2 3 4 5 6 7 8 9 140
                         1 2 3 4 5 6 7 8 9 150 #f ) )
               ))
;;; prints (720 (bar . -34) (1 2 ... #t))

;;; reload fib
(load "si/fib")

;;; prints 89.
(display ((global-value 'fib) 10))

;;; end of after.scm
