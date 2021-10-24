;;; $Id: chap8h.tst,v 4.0 1995/07/10 06:52:09 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Testing first class environments.

;;; create env
((lambda (a b) (export a))
 1 2 )
   ---
((lambda (a b c) (export b a c))
 1 2 3 )
   ---
(export car)
   ---

;;; some errors
(eval/b 3 4)
   *** ; 4 is not an environment

;;; use export on global variables
(eval/b 'car (export car))
   ---
((eval/b 'car (export car))
 '(a b) )
   a
((lambda (env) ((lambda (car)
                  (eval/b '(car (cons 1 2)) env) )
                cdr ))
 (export car) )
   1
;;; testing errors on globals
(export xyzzy)
   ---          ; xyzzy does not exist but is created on the fly
((lambda (a) (export a xyzzy))
 2 )
   ---          ; xyzzy does not exist but is created on the fly
((lambda (env) (eval/b 'xyzzy env))
 (export car) )
   ***          ; xyzzy is not initialized

;;; use export on local variables
((lambda (x env)
   ((lambda (a b) 
      (set! env (export a x))
      (set! x 2) )
    'aa 'bb )
   (eval/b '(set! x 3) env)
   (eval/b 'x env) )
 1 'envenv )
   3
((lambda (r)
   ((lambda (r1)
      ((lambda (env2)
         (eval/b '(set! x 2) env2)
         ((cdr r)) )
       ((car r)) ) )
    ((cdr r)) ) )
 ((lambda (x) (cons (lambda () (export x))
                    (lambda () x) ))
  1 ) )
   2
;;; defaults local to global
((lambda (env) ((eval/b 'car env) (cons 3 4)))
 ((lambda (a b) (export b))
  2 3 ) )
   3
((lambda (car) ((eval/b 'car car) (cons 3 4)))
 ((lambda (a b) (export b))
  2 3 ) )
   3
((lambda (env) (eval/b 'xyzzy env))
 ((lambda (a b) (export b))
  2 3 ) )
   ***       ; xyzzy is not initialized
((lambda (env) (eval/b 'xyzzy env))
 ((lambda (xyzzy b) (export b))
  2 3 ) )
   ***       ; xyzzy is not initialized

;;; Use (export) to grasp all the lexical env
((lambda (a b)
   ((lambda (a c)
      (export) )
    3 4 ) )
 1 2 )
    ---
((lambda (e) (eval/b '(list a b c)  e))
 ((lambda (a b)
    ((lambda (a c)
       (export) )
     3 4 ) )
  1 2 ) )
   (3 2 4)

;;; export in export to enrich it
((lambda (e) (eval/b '((lambda (x)
                         (export) )
                       5 )
                     e ))
 ((lambda (a b)
    ((lambda (a c)
       (export) )
     3 4 ) )
  1 2 ) )
   ---
((lambda (r) (eval/b '(list a b c x) r))
 ((lambda (e) (eval/b '((lambda (x)
                          (export) )
                        5 )
                      e ))
  ((lambda (a b)
     ((lambda (a c)
        (export) )
      3 4 ) )
   1 2 ) ))
   (3 2 4 5)
((lambda (r) (eval/b '(list a b c) r))
 ((lambda (e) (eval/b '((lambda (b)
                          (export) )
                        5 )
                      e ))
  ((lambda (a b)
     ((lambda (a c)
        (export) )
      3 4 ) )
   1 2 ) ))
   (3 5 4)


;;; Enrich an environment
(enrich 3 'foo)
   *** ; not an environment
(enrich (export car) 'foo)
   ---
(enrich (export car) 'foo 'bar 'wrek)
   ---
(enrich (export car) 3)
    *** ; Not a variable name
((lambda (e x)
   (set! e (enrich e 'x))
   (eval/b 'x e) )
 (export car) 3 )
   *** ;  X uninitialized
((lambda (e x car)
   (set! e (enrich e 'x))
   (eval/b '(set! x '(4)) e)
   (eval/b '(car x) e) )
 (export car) 3 cdr )
   4 
;;; Recursion through an enriched env
((lambda (e)
   (set! e (enrich (export) 'fact))
   (eval/b '(set! fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))) e)
   (eval/b '(fact 5) e) )
 'ee )
   120
;;; Mutual recursion
((lambda (e) 
   (set! e (enrich (export car) 'even? 'odd?))
   (eval/b '(set! even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) e)
   (eval/b '(set! odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))) e)
   (eval/b '(even? 4) e) )
 'ee )
   #t

;;; Testing procedure->definition
(procedure->definition (lambda (x) x))
   (lambda (x) x)
((lambda (f) (procedure->definition f))
 (lambda (x y) x y car) )
   (lambda (x y) x y car)

;;; Testing procedure->environment
(procedure->environment (lambda (x) x))
   ---
(procedure->environment car)
   *** ; not a closure
(eval/b 'x
        ((lambda (x)
           (procedure->environment (lambda () x)) )
         3 ) )
   3
;;; capture even the bindings that are not really used
(eval/b 'x
        ((lambda (x)
           (procedure->environment (lambda () car)) )
         3 ) )
   3
((lambda (p)
   (eval/b 'x (procedure->environment p)) )
 ((lambda (x) (lambda (y) car))
  4 ) )
   4

;;; Testing environment access
((variable-value 'car (export)) (cons 11 22))
   11
(begin (set! foo 33)
       (+ 1 (variable-value 'foo (export))) )
   34
((lambda (a)
   (cons (variable-value 'a (export)) 22) )
 11 )
   (11 . 22)
((lambda (a)
   (set! a (enrich (export) 'b))
   (eval/b '(set! b 33) a)
   (variable-value 'b a) )
 22 )
    33
((lambda (a) (variable-value 'b a))
 (enrich (export) 'b) )
   ***
((lambda (a) (variable-value 'foobar a))
 (enrich (export) 'b) )
   ***


;;; testing modification
(set-variable-value! 'car (export) 44)
   ***
(begin (set-variable-value! 'foo 
                            (begin (lambda () foo)
                                   (export) )
                            2 )
       foo )
   2
((lambda (a) 
   (set-variable-value! 'a (export) 22)
   a )
 11 )
   22
((lambda (a) 
   (set-variable-value! 'b a 22)
   (variable-value 'b a) )
 (enrich (export) 'b) )
   22
((lambda (a) (set-variable-value! 'foobar a 22))
 (export) )
   *** ; foobar does not exist.

;;; testing if a variable is defined in an environment
(variable-defined? 'car (export))
   #t
(begin (set! foo 3)
       (variable-defined? 'foo (export)) )
   #t
(variable-defined? 'foobar (export car))
  #f
((lambda (a) (variable-defined? 'a (export)))
 33 )
   #t
((lambda (e) (variable-defined? 'b e))
 (enrich (export) 'b) )
   #t
((lambda (a) (variable-defined? 'foobar (export a car)))
 33 )
   #f

;;; end of chap8h.tst
