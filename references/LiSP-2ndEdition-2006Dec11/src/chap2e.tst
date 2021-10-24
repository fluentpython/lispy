;;; $Id: chap2e.tst,v 4.0 1995/07/10 06:51:02 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

33
   33
xyy 
   *** ; unexistant
'foo
   foo
(if #t 1 2)
   1
(if #f 2 3)
   3
(begin 1 2 3)
   3
(begin (set! a 3) a)
   3
(cons 3 4)
   (3 . 4)
((lambda (x y) (cons x y))
 1 2 )
   (1 . 2)
cons 
   *** ; cons not a variable
((lambda (f) (f 1 2))
 cons )
   *** ; cons not a variable
(apply (function (lambda (x y) (cons y x))) '1 '2 '())
   (2 . 1)

; no computation in functional position
((if #t cons list) 1 22)
   ***

;;; dynamic variables default to global variables
(begin (set! foo 33)
       foo )
   33
; a surrounding dynamic is no longer needed
(dynamic-let ((foo (* 2 3)))
     foo )
   6
;; wrong since the first reference to foo is global and not dynamic
;(begin (set! bar (function (lambda () foo)))
;       (dynamic-let ((foo (* 2 3)))
;           (apply bar '()) ) )
;   6
(dynamic-let ((foo (* 2 3)))
  (set! bar (function (lambda () foo)))
  (apply bar '()) )
   6
(dynamic-let ((foo (* 2 3)))
  (set! bar (function (lambda () foo)))
  (dynamic-let ((foo 44))
     (apply bar '()) ) )
   44
(dynamic-let ((foo (* 2 3)))
   (set! foo (* 4 4))
   (list (dynamic-let ((foo 55))
             (set! foo (* 5 5))
             foo )
         foo ) )
   (25 16)

;;; example from book
;(dynamic-let ((x 2))
;   (+ x                        ; dynamic
;      (let ((x (+              ; lexical
;                  x x )))      ; dynamic
;        (+ x                   ; lexical
;           (dynamic x) ) ) ) ) ; dynamic
;   8
(dynamic-let ((x 2))
   (+ x                                 ; dynamic
      ((lambda (x) (+ x                 ; lexical
                      (dynamic x) ))    ; dynamic
       (+ x x) ) ) )                    ; dynamic 
   8

;;; end of chap2e.tst
