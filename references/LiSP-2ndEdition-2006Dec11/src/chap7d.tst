;;; $Id: chap7d.tst,v 4.0 1995/07/10 06:51:52 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Tests on tail-rec-ness and bind-exit and error handlers.

;;; Verifying tail rec (stack is actually 100 long)

((lambda (loop)
   (set! loop (lambda (n) (if (> n 0) (loop (- n 1)) n)))
   (loop 10) )
 #f )
   0
((lambda (loop)
   (set! loop (lambda (n) (if (> n 0) (loop (- n 1)) n)))
   (loop 200) )
 #f )
   0
((lambda (loop)
   (set! loop (lambda (n k) 
                (if (> n 0)
                    (call/cc (lambda (kk) (loop (- n 1) k)))
                    (k n) ) ))
   (loop 10 (lambda (x) x)) )
  #f )
  0
((lambda (loop)
   (set! loop (lambda (n k) 
                (if (> n 0)
                    (call/cc (lambda (kk) (loop (- n 1) k)))
                    (k n) ) ))
   (loop 200 (lambda (x) x)) )
  #f )
  ---

;;; Testing bind-exit

(bind-exit (k) 1)
   1
(bind-exit (k) (k 'a))
   a
(bind-exit (k) (cons (k 'a) 1))
   a
(bind-exit (k) k)
   ---
(bind-exit (k) 
  ((lambda (kk) (+ 1 (kk 'z)))
   k )
  (k 'a) )
   z
(bind-exit (k1)
  (* 2 (bind-exit (k2)
          (k1 3) )) )
   3
(bind-exit (k1)
  (* 2 (bind-exit (k2)
          (+ 5 (k1 3)) )) )
   3
(bind-exit (k1)
  (* 2 (bind-exit (k2)
          (+ 5 (k2 3)) )) )
   6
(bind-exit (k1)
  (* 2 ((bind-exit (k2)
          (+ 5 (k2 k1)) )
        3 )) )
   3
;;; restaure lexical env
((lambda (a)
   (bind-exit (k) a) )
 11 )
   11
((lambda (a)
   (bind-exit (k) a)
   a )
 11 )
   11
((lambda (a)
   (bind-exit (k)
      ((lambda (b) a)
       (cons a a) ) ) )
 11 )
   11
;;; out of extent
((bind-exit (k) k) 33)
   ***
;;; exercize some errors when invoking continuations
(bind-exit (k) (k))
   ***
(bind-exit (k) (k 1 2))
   ***
((bind-exit (k) k) 1 2 3)
   ***
(bind-exit (k) (cons 1 (apply k 2 '())))
   2
(bind-exit (k) (cons 1 (apply k '(3))))
   3
;;; variable arity on continuations
(bind-exit (k) (cons 1 (apply k 1 2 '())))
   ***
(bind-exit (k) (cons 1 (apply k '())))
   ***
((lambda (kk)
   (bind-exit (k1)
      (* 2 (bind-exit (k2)
              (set! kk k2)
              (k1 1) )) )
   (kk 3) )
 'wait )
   ***

;;; Testing error handlers.

(monitor (lambda (continuable? anomaly) 3)
         1 )
   1
(monitor (lambda (continuable? anomaly) 3)
         1 2 4 )
   4
foo
   ***
(monitor (lambda (continuable? anomaly) 3)
         foo )
   3
(pair? (monitor list
                foo ))
   #t
(monitor (lambda (continuable? anomaly) 2)
         (+ 1 foo) )
   3
bar
   ***
(monitor (lambda (continuable? anomaly) bar)
         foo )
   ***
;;; (apply cons) is not continuable?
(monitor (lambda (continuable? anomaly) 33)
         (apply cons) )
   ***
(bind-exit (k)
  (monitor (lambda (continuable? anomaly) (k 44))
    (monitor (lambda (continuable? anomaly) 33)
        (apply cons) ) ) )
   44
;;; show that the handler is called in the right dynamic environment
;(monitor (lambda (c e) ((dynamic foo) 1))
;  (let fact ((n 5))
;    (if (= n 0) (/ 11 0)
;        (* n (bind-exit (k)
;               (dynamic-let (foo k)
;                  (fact (- n 1)) ) )) ) ) )
(monitor (lambda (c e) ((dynamic foo) 1))
  ((lambda (fact)
     (begin (set! fact (lambda (n)
                         (if (= n 0) bar
                             (* n (bind-exit (k)
                                    (dynamic-let (foo k)
                                      (fact (- n 1)) ) )) ) ))
            (fact 5) ) )
   'void ) )
   120

;;; combining dynamics and escapes. show that shallow binding without
;;; unwind-protect looses.  
"shallow binding without unwind-protect when escaping"
   ---
(dynamic-let (a 1)
   (bind-exit (k)
      (dynamic-let (a 2)
        (k 'out) ) )
   "shallow binding returns 2 instead of 1!!! "
   (dynamic a) )
   1 

;;; end of chap7d.tst
