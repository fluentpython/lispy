;;; $Id: chap8a.tst,v 4.0 1995/07/10 06:52:00 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Tests on eval (in current lexical environment)

(eval 'car)
   ---
((eval 'car) '(a b c))
   a
((eval '(lambda (x) (list x x))) (+ 2 3))
   (5 5)
(eval 345)
   345

;;; with side effects on the global mutable environment
;;; Foo and bar already exist
(begin (set! foo 2)
       (eval '(set! foo 33))
       foo )
   33
(begin (set! bar 33)
       (set! foo 44)
       (eval '(set! bar (- foo bar)))
       bar )
   11

;;; More than one eval
((lambda (loop fact)
   (set! loop (lambda (i)
                (if (> i 0)
                    (begin (display (eval (list fact fact 10)))
                           (loop (- i 1)) )
                    'done ) ))
   (loop 0) )
 'loop '(lambda (f n) (if (= n 0) 1 (* n (f f (- n 1))))) )
   done

((eval '(lambda (x) (cons x x)))
 33 )
 (33 . 33) 
(((eval '(lambda (f) 
           (lambda (x) (f x)) ))
  list )  
 44 )    
   (44) 

;;; Exercizing some errors. No test on syntax since interpreters of the 
;;; book neglect this aspect.
(eval car)
   ***
;;; remove this test since transcode-back cannot handle circular structures.
;(eval ((lambda (a)
;         (set-car! (cdr (cdr a)) a)
;         a )
;       (list 'if #f 2  3) ))
;   ***

;;; examples in the book
(begin (set! x 2)
       (set! z 1)
       (display
        (list ((lambda (x) (eval 'x))
               3 )
              ((lambda (x y) (eval y))
               4 (list 'eval 'x) )
              ((lambda (x y z) (eval y))
               5 (list 'eval 'z) 'x ) ) ) )
   ---
;;; (3 4 5) if special form
;;; (2 2 1) if function


" !!!!!!!!!!!!  eval as a function will fail at the next one !!!!!!!!!!!!! "
   ---

;;; Using local lexical environment
((lambda (x) (eval 'x))
 22 )
   22
((lambda (x y) (eval y))
 3 'x )
   3
((lambda (x y) (eval y))
 4 (list 'eval 'x) )
   4
((lambda (x y z) (eval y))
 5 (list 'eval 'z) 'x )
   5

;;; Dynamic creation of global variables
(eval '(begin (set! wrek 33) wrek))
   33

;;; end of chap8a.tst
