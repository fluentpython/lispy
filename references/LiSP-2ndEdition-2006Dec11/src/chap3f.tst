;;; $Id: chap3f.tst,v 4.0 1995/07/10 06:51:15 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

(block foo 33)
   33
(block foo 1 2 3)
   3
(block foo (+ 1 (return-from foo 2)))
   2
(return-from foo 3)
   *** ; no block foo
((block foo (lambda (x) (return-from foo x)))
 3 )
   *** ; obsolete block foo
((block a 
   (* 2 (block b (return-from a (lambda (x) (return-from b x))))) )
 3 )
   *** ; obsolete block b
((block a 
   (* 2 (block b (return-from a (lambda (x) (return-from a x))))) )
 3 )
   *** ; obsolete block a
; scope of block labels
(block foo 
  ((lambda (exit)
    (* 2 (block foo
            (* 3 (exit 5)) )) )
    (lambda (x) (return-from foo x)) ) )
   5

(catch 'bar 1)
   1
(catch 'bar 1 2 3)
   3
(throw 'bar 33)
   *** ; no catcher for bar
(catch 'bar (throw 'bar 11))
   11
(catch 'bar (* 2 (throw 'bar 5)))
   5
((lambda (f) 
   (catch 'bar (* 2 (f 5))) )
 (lambda (x) (throw 'bar x)) )
   5
((lambda (f) 
   (catch 'bar (* 2 (catch 'bar (* 3 (f 5))))) )
 (lambda (x) (throw 'bar x)) )
   10
(catch 2
  (* 7 (catch 1
         (* 3 (catch 2
                (throw 1 (throw 2 5)) )) )) )
   105
(catch 2 (* 7 (throw 1 (throw 2 3))))
   *** ; no catcher for 1

(unwind-protect 1 2)
  1
((lambda (c)
   (unwind-protect 1 (set! c 2))
   c )
 0 )
   2
((lambda (c)
   (catch 111 (* 2 (unwind-protect (* 3 (throw 111 5))
                      (set! c 1) ))) )
  0 )
   5
((lambda (c)
   (catch 111 (* 2 (unwind-protect (* 3 (throw 111 5))
                      (set! c 1) )))
   c )
  0 )
   1
((lambda (c)
   (block A (* 2 (unwind-protect (* 3 (return-from A 5))
                      (set! c 1) ))) )
 0 )
   5
((lambda (c)
   (block A (* 2 (unwind-protect (* 3 (return-from A 5))
                      (set! c 1) )))
   c )
 0 )
   1

(catch 1 (catch 2 (unwind-protect (throw 1 'foo) (throw 2 'bar) ) ) )
   ---


;;; throw as a function
(set! funcall (lambda (g . args) (apply g args)))
   ---
(funcall throw 'bar 33)
   *** ; no catcher for bar
(catch 'bar (funcall throw 'bar 11))
   11
(catch 'bar (* 2 (funcall throw 'bar 5)))
   5
((lambda (f) 
   (catch 'bar (* 2 (f 5))) )
 (lambda (x) (funcall throw 'bar x)) )
   5
((lambda (f) 
   (catch 'bar (* 2 (catch 'bar (* 3 (f 5))))) )
 (lambda (x) (funcall throw 'bar x)) )
   10
(catch 2
  (* 7 (catch 1
         (* 3 (catch 2
                (funcall throw 1 (funcall throw 2 5)) )) )) )
   105
(catch 2 (* 7 (funcall throw 1 (funcall throw 2 3))))
   3

;;; end of chap3f.tst
