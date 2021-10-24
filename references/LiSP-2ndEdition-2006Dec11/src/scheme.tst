;;; $Id: scheme.tst,v 1.12 1998/01/11 08:45:22 queinnec Exp $

;;;                       A test suite for Scheme.

;;; All tests are written in pure normalized Scheme: they do not use
;;; extra syntaxes (no let, no letrec), alternatives are always
;;; ternary, bodies of lambda are unary. All expressions are
;;; independent and form stand-alone programs (they can be
;;; independently compiled). Most of these tests are now independent
;;; of the evaluation order.  Tests on call/cc are at the end of this
;;; test suite (use of continuations out of their dynamic extent
;;; follow more normal uses and are preceded by a comment).

;;; These tests suppose that variables t, f and nil exist and are
;;; bound to #t, #f and ().  They also suppose that variables a, foo,
;;; fact and primes also exist. On the contrary, the `xyzzy' variable
;;; is supposed not to exist.

t
   #t
f
   #f
nil
   ()
xyzzy
   ***
444
   444
'foo
   foo
'33
   33
"Foo"
   "Foo"
(if t 1 2)
   1
(if f 1 2)
   2
(begin 1)
   1
(begin 1 2)
   2
(begin 1 2 3 4)
   4
(begin (set! a 1) a)
   1
(begin (set! a 1) (set! a 2) a)
   2
(set! xyzzy 3)
   ***
(begin (begin (set! a 1) 2)
       a )
   1
(lambda () t)
   ---
(lambda (x) x)
   ---
(lambda (x y) (cons x y))
   ---
((lambda (x) x) 1)
   1
((lambda (x y) y) 1 2)
   2
((lambda (x y) xyzzy) 1 2)
   ***
(cons 33 44)
   (33 . 44)
((lambda (x y) (cons y x)) 1 2)
   (2 . 1)
(((lambda (z) (lambda (x) (cons x z))) 2) 1)
   (1 . 2)
(((lambda (z) (lambda (x) (cons x z))) (cons 2 '())) 1)
   (1 2)
(((lambda (z) (lambda (x) (cons x z))) (cons 2 (cons 3 '()))) 1)
   (1 2 3)
(lambda (x) (set! x (cons x x)))
   ---
(lambda (x y) (begin (set! x y) (set! y x)))
   ---
((lambda (x y) (begin (set! x y) x)) 1 2)
   2
((lambda (x y z) (begin (set! x (cons x z))
                        (set! y (cons y x))
                        y ))
 1 2 3 )
   (2 . (1 . 3))
;;; non-remanence of variables
(begin
  (set! foo 0)
  ((lambda (foo)
     foo )
   1 )
  foo )
   0
(begin
  (set! foo 0)
  ((lambda (foo)
     (set! foo 2) )
   1 )
  foo )
   0
(((lambda (car) car)
  cdr )
 '(a b c) )
   (b c)
((lambda (y)
   ((lambda (f) (f f 0))
    (lambda (f x)
      (cons y ((lambda (y)
                 (if (= x 1) x (f f (+ x 1))) )
               (+ x 1) )) ) ) )
 5 )
   (5 5 . 1)
;;; multiple levels of lambda
((lambda (bar) 
   (lambda (k) 
     (lambda (r) (k 4)) ) )
 (quote bar))
   ---
(begin
  (set! foo
        ((lambda (z)
           (lambda (x) (begin (set! z (cons x z))
                              z )) )
         '() ) )
  (set! a '())
  (set! a (cons (foo 1) a))
  (set! a (cons (foo 2) a))
  (set! a (cons (foo 3) a))
  a )
   ((3 2 1)
    (2 1)
    (1) )
((lambda (x y) (cons x y)))
   ***
((lambda (x y) (cons x y)) 1)
   ***
((lambda (x y) (cons x y)) 2 3 4)
   ***

;;; Test more and more complex quotations.
;;; There was a problem with ~/DEA/book/src/chap4a.scm (evaluate-memo-quote)
;;; but I think I fixed it.
'("foo" #f #t () bar 23)
   ("foo" #f #t () bar 23)
'(((((() . "foo") . #f) . #t) . bar) . 23)
   (((((() . "foo") . #f) . #t) . bar) . 23)
;;; With possible sharing (pay attention to case sensitivity)
'((a "a" . "A") a "a" . "A")
   ((a "a" . "A") a "a" . "A")
'((a . b) a . b)
   ((a . b) a . b)

;;; testing some primitives
(cons 'a 'b)
   (a . b)
(cons)
   ***
(cons 'a)
   ***
(cons 'a 'b 'c 'd)
   ***
(car (cons 'a 'b))
   a
(cdr (cons 'a 'b))
   b
(eq? 'a 'b)
   #f
(eq? 'a 'a)
   #t
(eq? (car '(a b)) (car (cons 'a 'b)))
   #t
(begin (set! a (cons 'a 'b)) a)
   (a . b)
(begin (set! a (cons 'a 'b)) (set-car! a 'c) a)
   (c . b)
(begin (set! a (cons 'a 'b)) (set-cdr! a 'd) a)
   (a . d)
(symbol? 'a)
   #t
(symbol? '(1))
   #f

;;; testing boolean values
(if (pair? 1) 2 3)
   3
(if (pair? (cons 1 2)) 3 4)
   3

;;; begin evaluates only once its parameters
(begin (set! a 1) (set! a (cons a a)) a)
   (1 . 1)
   
((lambda (v f) (f v v))
 1 cons )
   (1 . 1)
((lambda (v f) (f v v))
 1 (lambda (x y) (cons x y)) )
   (1 . 1)

(* 2 3)
   6
(- 5 3)
   2
(< 2 4)
   #t
(< 2 1)
   #f
(< 2 2)
   #f

;;; Small programs.
((lambda (fact)
   (begin (set! fact (lambda (n)
                       (if (< n 2) 1
                           (* n (fact (- n 1))) ) ))
          (fact 5) ) )
 'fact )
   120
((lambda (fib)
   (begin (set! fib 
                (lambda (n)
                  (if (<= n 2) 1
                      (+ (fib (- n 1)) (fib (- n 2))) ) ) )
          (fib 6) ) )
 'fib )
   8
((lambda (bar fact)
   (begin 
     (set! bar 
           (lambda (n k)
             (if (< n 2) (k 1)
                 (bar (- n 1) (lambda (r) (k (* n r)))) ) ) )
     (begin (set! fact (lambda (n) (bar n (lambda (x) x))))
            (fact 5) ) ) )
 'bar 'fact )
   120
((lambda (fact)
   (begin (set! fact (lambda (n f)
                       (if (< n 2) 1
                           (* n (f (- n 1) f)) ) ))
          (fact 5 fact) ) )
 'fact )
   120

;;; A little letrec by hand, using the `fact' global variable.
(begin
  (set! fact 
        (lambda (n)
          ((lambda (factint)
             (begin (set! factint 
                          (lambda (n f)
                            (if (< n 2) 1
                                (* n (f (- n 1) f)) ) ) )
                    (factint n factint) ) )
           'wait ) ) )
  (cons (fact 1) (cons (fact 2) (cons (fact 5) '()))) )
(1 2 120)

;;; Another hand-made letrec with more closures closing variables at
;;; different depths.
((lambda (primes filter)  
   (set! primes
         (lambda (n f max)
           (if (> n max)
               '()
               (if (f n)
                   (primes (+ n 1) f max)
                   (cons n 
                         ((lambda (ff)
                            (primes (+ n 1)
                                    (lambda (p) (if (f p) t (ff p)))
                                    max ) )
                          (filter n) ) ) ) ) ) )
   ;; at the beginning no number is composite (all numbers are prime).
   (primes 2 (lambda (x) f) 10) )
 'wait
 ;; this predicate returns true if n is a composite number ie a multiple of p.
 (lambda (p) (lambda (n) (= 0 (remainder n p)))) )
   (2 3 5 7)

;;; Same example using the `primes' global variable.
(begin
  (set! primes 
        (lambda (n f max)
          ((lambda (filter)
             (begin
               ;; this predicate returns true if n is a composite number
               ;; ie a multiple of p.
               (set! filter (lambda (p)
                              (lambda (n) 
                                (= 0 (remainder n p))) ))
               (if (> n max)
                   '()
                   (if (f n)
                       (primes (+ n 1) f max)
                       (cons n 
                             ((lambda (ff)
                                (primes (+ n 1)
                                        (lambda (p) (if (f p) t (ff p)))
                                        max ) )
                              (filter n) ) ) ) ) ) )
           'wait ) ) )
  ;; at the beginning no number is composite (all numbers are prime).
  (primes 2 (lambda (x) f) 10) )
   (2 3 5 7)

;;; Tests assignments in various positions
((lambda (x y)
   (begin (set! x (cons x y))
          (set! y (car x))
          (set! x (cdr x))
          (cons x y) ) )
 'a 'b )
   (b . a)
(((lambda (x) (lambda (y) (begin (set! x y) (cons x y))))
  'x-value )
 'y-value )
   (y-value . y-value)
(((lambda (x) (lambda (y) (begin (set! x (cons x y)) (cons x y))))
  'x-value )
 'y-value )
   ((x-value . y-value) . y-value)
((lambda (ff) ((car ff)))
 ((lambda (x) (cons (lambda () x)
                    (lambda (y) (set! x y)) ))
  'x-value ) )
    x-value
((lambda (ff) (begin ((cdr ff) 'y-value) ((car ff))))
 ((lambda (x) (cons (lambda () x)
                    (lambda (y) (set! x y)) ))
  'x-value ) )
    y-value
((lambda (x) (x 'y))
 ((lambda (x) (lambda (y) x))
  'x-value ) )
    x-value
(((lambda (x)
    ((lambda (x)
       (lambda () x) )
     (cons x x) ) )
  'x-value ))
   (x-value . x-value)

;;; testing environment restauration
;;; in an inlined application to cons.
((lambda (a b)
   (begin (cons (set! a 33)
                (set! b 44) )
          (cons a b) ) )
 11 22 )
   (33 . 44)
((lambda (a b)
   ((lambda (f)
      (begin (f 33)
             (cons a b) ) )
    (lambda (x) (cons (set! a x) (set! b x))) ) )
 11 22 )
   (33 . 33)
((lambda (a b)
   ((lambda (f)
      (begin (f (begin (f 33) 44))
             (cons a b) ) )
    (lambda (x) (cons (set! a x) (set! b x))) ) )
 11 22 )
   (44 . 44)

;;; Various apply forms:
apply
   ---
(apply)
   ***
(apply cons)
   ***
(apply cons '(1 2))
   (1 . 2)
(apply cons '(1 2 3))
   ***
(apply cons '(1))
   ***
(apply cons 1 '(2))
   (1 . 2)
(apply cons 1 2 '())
   (1 . 2)

;;; variable arity functions:
list
   ---
(list 1 2 3)
   (1 2 3)
(list 1)
   (1)
(list)
   ()
(apply list '(1 2 3))
   (1 2 3)
(apply list 1 2 '(3 4))
  (1 2 3 4)
(apply apply cons (list 1 2 '()))
   (1 . 2)

;;; Testing numerous arities
(begin (set! foo (lambda () 9))
       (foo) )
   9
(begin (set! foo (lambda () 9))
       (foo 1) )
   ***
(begin (set! foo (lambda () 9))
       (foo 1 2) )
   ***
(begin (set! foo (lambda () 9))
       (foo 1 2 3) )
   ***
(begin (set! foo (lambda () 9))
       (foo 1 2 3 4) )
   ***
(begin (set! foo (lambda () 9))
       (foo 1 2 3 4 5) )
   ***
(begin (set! foo (lambda (z) z))
       (foo) )
   ***
(begin (set! foo (lambda (z) z))
       (foo 1) )
   1
(begin (set! foo (lambda (z) z))
       (foo 1 2) )
   ***
(begin (set! foo (lambda (z) z))
       (foo 1 2 3) )
   ***
(begin (set! foo (lambda (z) z))
       (foo 1 2 3 4) )
   ***
(begin (set! foo (lambda (z) z))
       (foo 1 2 3 4 5) )
   ***
(begin (set! foo (lambda (a z) z))
       (foo) )
   ***
(begin (set! foo (lambda (a z) z))
       (foo 1) )
   ***
(begin (set! foo (lambda (a z) z))
       (foo 1 2) )
   2
(begin (set! foo (lambda (a z) z))
       (foo 1 2 3) )
   ***
(begin (set! foo (lambda (a z) z))
       (foo 1 2 3 4) )
   ***
(begin (set! foo (lambda (a z) z))
       (foo 1 2 3 4 5) )
   ***
(begin (set! foo (lambda (a b z) z))
       (foo) )
   ***
(begin (set! foo (lambda (a b z) z))
       (foo 1) )
   ***
(begin (set! foo (lambda (a b z) z))
       (foo 1 2) )
   ***
(begin (set! foo (lambda (a b z) z))
       (foo 1 2 3) )
   3
(begin (set! foo (lambda (a b z) z))
       (foo 1 2 3 4) )
   ***
(begin (set! foo (lambda (a b z) z))
       (foo 1 2 3 4 5) )
   ***
(begin (set! foo (lambda (a b c z) z))
       (foo) )
   ***
(begin (set! foo (lambda (a b c z) z))
       (foo 1) )
   ***
(begin (set! foo (lambda (a b c z) z))
       (foo 1 2) )
   ***
(begin (set! foo (lambda (a b c z) z))
       (foo 1 2 3) )
   ***
(begin (set! foo (lambda (a b c z) z))
       (foo 1 2 3 4) )
   4
(begin (set! foo (lambda (a b c z) z))
       (foo 1 2 3 4 5) )
   ***
(begin (set! foo (lambda (a b c d z) z))
       (foo) )
   ***
(begin (set! foo (lambda (a b c d z) z))
       (foo 1) )
   ***
(begin (set! foo (lambda (a b c d z) z))
       (foo 1 2) )
   ***
(begin (set! foo (lambda (a b c d z) z))
       (foo 1 2 3) )
   ***
(begin (set! foo (lambda (a b c d z) z))
       (foo 1 2 3 4) )
   ***
(begin (set! foo (lambda (a b c d z) z))
       (foo 1 2 3 4 5) )
   5
(begin (set! foo (lambda (a b c d e z) z))
       (foo) )
   ***
(begin (set! foo (lambda (a b c d e z) z))
       (foo 1) )
   ***
(begin (set! foo (lambda (a b c d e z) z))
       (foo 1 2) )
   ***
(begin (set! foo (lambda (a b c d e z) z))
       (foo 1 2 3) )
   ***
(begin (set! foo (lambda (a b c d e z) z))
       (foo 1 2 3 4) )
   ***
(begin (set! foo (lambda (a b c d e z) z))
       (foo 1 2 3 4 5) )
   ***
(begin (set! foo (lambda (a b c d e z) z))
       (foo 1 2 3 4 5 6) )
   6

;; testing the conversion of an application to a closed application.
((lambda () 1))
1
((lambda (x) (cons x x)) 2)
(2 . 2)
((lambda (x y) (cons x y)) 3 4)
(3 . 4)
((lambda (x y z) z) 1 2 3)
3
((lambda (a b c d) d) 1 2 3 4)
4
((lambda (a b c d e) e) 1 2 3 4 5)
5
(((lambda (x) (lambda (a) x)) 0) 1)
0
(((lambda (x) (lambda (a b) x)) 0) 1 2)
0
(((lambda (x) (lambda (a b c) x)) 0) 1 2 3)
0
(((lambda (x) (lambda (a b c d) x)) 0) 1 2 3 4)
0
(((lambda (x) (lambda (a b c d e) x)) 0) 1 2 3 4 5)
0

;;; testing non immediate closure of values
((((lambda (x)
     (lambda ()
       (lambda (y) (cons x y)) ) )
   'x ))
 'y )
   (x . y)

;;; Testing homonymy
((lambda (x) 
   ((lambda (x) (+ x 1))
    (* 2 x) ) )
 5 )
   11
((lambda (x)
   (x 3) )
 (lambda (x) (* x x)) )
   9
((lambda (x)
   ((lambda (x) 
      (x) )
    (lambda () x) ) )
 3 )
    3
((lambda (x)
   ((lambda (x) 
      ((lambda (x) 
         ((car x)) )
       (cons x x) ) )
    (lambda () (+ 1 x)) ) )
 3 )
   4
(((lambda (x)
    ((lambda (x) 
       ((lambda (x) 
          (lambda () 
            ((car x) 2) ) )
        (list x) ) )
     (lambda (y) (* y x)) ) )
  3 ))
   6
;;; Testing coalescence of let forms
(list ((lambda (x) (cons x x))
       33 )
      ((lambda (x) (list x x))
       44 ) )
   ((33 . 33)(44 44))
(list ((lambda (x) (cons x x))
       33 )
      ((lambda (x) (set! x 55) (list x x))
       44 ) )
   ((33 . 33)(55 55))
(list ((lambda (x) (cons x x))
       33 )
      ((lambda (x) (set! x 55) (list x x))
       44 )
      ((lambda (x) (list x x))
       66 ) )
   ((33 . 33)(55 55)(66 66))

;;; testing variable arity through apply 
(apply (lambda (z) z) 1 '())
   1
(apply (lambda (z) z) '(1))
   1
(apply (lambda (z) z) '(1 2 3 4 5 6 7))
   ***
(apply (lambda (z) z) 1 '(2 3 4 5 6 7))
   ***
(apply (lambda (z) z) 1 2 '(3 4 5 6 7))
   ***
(apply (lambda (z) z) 1 2 3 '(4 5 6 7))
   ***
(apply (lambda (z) z) 1 2 3 4 '(5 6 7))
   ***
(apply (lambda (z) z) 1 2 3 4 5 '(6 7))
   ***
(apply (lambda (z) z) '())
   ***
;;;
(apply (lambda (a z) z) '(1 2))
   2
(apply (lambda (a z) z) 1 '(2))
   2
(apply (lambda (a z) z) 1 2 '())
   2
(apply (lambda (a z) z) '(1 2 3 4 5 6 7))
   ***
(apply (lambda (a z) z) 1 '(2 3 4 5 6 7))
   ***
(apply (lambda (a z) z) 1 2 '(3 4 5 6 7))
   ***
(apply (lambda (a z) z) 1 2 3 '(4 5 6 7))
   ***
(apply (lambda (a z) z) 1 2 3 4 '(5 6 7))
   ***
(apply (lambda (a z) z) 1 2 3 4 5 '(6 7))
   ***
(apply (lambda (a z) z) '())
   ***
(apply (lambda (a z) z) '(1))
   ***
;;;
(apply (lambda (a b z) z) '(1 2 3))
   3
(apply (lambda (a b z) z) 1 '(2 3))
   3
(apply (lambda (a b z) z) 1 2 '(3))
   3
(apply (lambda (a b z) z) 1 2 3 '())
   3
(apply (lambda (a b z) z) '(1 2 3 4 5 6 7))
   ***
(apply (lambda (a b z) z) 1 '(2 3 4 5 6 7))
   ***
(apply (lambda (a b z) z) 1 2 '(3 4 5 6 7))
   ***
(apply (lambda (a b z) z) 1 2 3 '(4 5 6 7))
   ***
(apply (lambda (a b z) z) 1 2 3 4 '(5 6 7))
   ***
(apply (lambda (a b z) z) 1 2 3 4 5 '(6 7))
   ***
(apply (lambda (a b z) z) '())
   ***
(apply (lambda (a b z) z) '(1))
   ***
(apply (lambda (a b z) z) '(1 2))
   ***
;;;
(apply (lambda (a b c z) z) '(1 2 3 4))
   4
(apply (lambda (a b c z) z) 1 '(2 3 4))
   4
(apply (lambda (a b c z) z) 1 2 '(3 4))
   4
(apply (lambda (a b c z) z) 1 2 3 '(4))
   4
(apply (lambda (a b c z) z) 1 2 3 4 '())
   4
(apply (lambda (a b c z) z) '(1 2 3 4 5 6 7))
   ***
(apply (lambda (a b c z) z) 1 '(2 3 4 5 6 7))
   ***
(apply (lambda (a b c z) z) 1 2 '(3 4 5 6 7))
   ***
(apply (lambda (a b c z) z) 1 2 3 '(4 5 6 7))
   ***
(apply (lambda (a b c z) z) 1 2 3 4 '(5 6 7))
   ***
(apply (lambda (a b c z) z) 1 2 3 4 5 '(6 7))
   ***
(apply (lambda (a b c z) z) '())
   ***
(apply (lambda (a b c z) z) '(1))
   ***
(apply (lambda (a b c z) z) '(1 2))
   ***
(apply (lambda (a b c z) z) '(1 2 3))
   ***
;;;
(apply (lambda (a b c d z) z) '(1 2 3 4 5))
   5
(apply (lambda (a b c d z) z) 1 '(2 3 4 5))
   5
(apply (lambda (a b c d z) z) 1 2 '(3 4 5))
   5
(apply (lambda (a b c d z) z) 1 2 3 '(4 5))
   5
(apply (lambda (a b c d z) z) 1 2 3 4 '(5))
   5
(apply (lambda (a b c d z) z) 1 2 3 4 5 '())
   5
(apply (lambda (a b c d z) z) '(1 2 3 4 5 6 7))
   ***
(apply (lambda (a b c d z) z) 1 '(2 3 4 5 6 7))
   ***
(apply (lambda (a b c d z) z) 1 2 '(3 4 5 6 7))
   ***
(apply (lambda (a b c d z) z) 1 2 3 '(4 5 6 7))
   ***
(apply (lambda (a b c d z) z) 1 2 3 4 '(5 6 7))
   ***
(apply (lambda (a b c d z) z) 1 2 3 4 5 '(6 7))
   ***
(apply (lambda (a b c d z) z) '())
   ***
(apply (lambda (a b c d z) z) '(1))
   ***
(apply (lambda (a b c d z) z) '(1 2))
   ***
(apply (lambda (a b c d z) z) '(1 2 3))
   ***
(apply (lambda (a b c d z) z) '(1 2 3 4))
   ***
;;;
(apply (lambda (a b c d e z) z) '(1 2 3 4 5 6))
   6
(apply (lambda (a b c d e z) z) 1 '(2 3 4 5 6))
   6
(apply (lambda (a b c d e z) z) 1 2 '(3 4 5 6))
   6
(apply (lambda (a b c d e z) z) 1 2 3 '(4 5 6))
   6
(apply (lambda (a b c d e z) z) 1 2 3 4 '(5 6))
   6
(apply (lambda (a b c d e z) z) 1 2 3 4 5 '(6))
   6
(apply (lambda (a b c d e z) z) 1 2 3 4 5 6 '())
   6
(apply (lambda (a b c d e z) z) '(1 2 3 4 5 6 7))
   ***
(apply (lambda (a b c d e z) z) 1 '(2 3 4 5 6 7))
   ***
(apply (lambda (a b c d e z) z) 1 2 '(3 4 5 6 7))
   ***
(apply (lambda (a b c d e z) z) 1 2 3 '(4 5 6 7))
   ***
(apply (lambda (a b c d e z) z) 1 2 3 4 '(5 6 7))
   ***
(apply (lambda (a b c d e z) z) 1 2 3 4 5 '(6 7))
   ***
(apply (lambda (a b c d e z) z) '())
   ***
(apply (lambda (a b c d e z) z) '(1))
   ***
(apply (lambda (a b c d e z) z) '(1 2))
   ***
(apply (lambda (a b c d e z) z) '(1 2 3))
   ***
(apply (lambda (a b c d e z) z) '(1 2 3 4))
   ***
(apply (lambda (a b c d e z) z) '(1 2 3 4 5))
   ***
;;;
(apply (lambda (a b c d e f z) z) '(1 2 3 4 5 6 7))
   7
(apply (lambda (a b c d e f z) z) 1 '(2 3 4 5 6 7))
   7
(apply (lambda (a b c d e f z) z) 1 2 '(3 4 5 6 7))
   7
(apply (lambda (a b c d e f z) z) 1 2 3 '(4 5 6 7))
   7
(apply (lambda (a b c d e f z) z) 1 2 3 4 '(5 6 7))
   7
(apply (lambda (a b c d e f z) z) 1 2 3 4 5 '(6 7))
   7
(apply (lambda (a b c d e f z) z) 1 2 3 4 5 6 '(7))
   7
(apply (lambda (a b c d e f z) z) 1 2 3 4 5 6 7 '())
   7
(apply (lambda (a b c d e f z) z) '(1 2 3 4 5 6 7 8))
   ***
(apply (lambda (a b c d e f z) z) '(1 2 3 4 5 6))
   ***
(apply (lambda (a b c d e f z) z) '(1 2 3 4 5))
   ***
(apply (lambda (a b c d e f z) z) '(1 2 3 4))
   ***
(apply (lambda (a b c d e f z) z) '(1 2 3))
   ***
(apply (lambda (a b c d e f z) z) '(1 2))
   ***
(apply (lambda (a b c d e f z) z) '(1))
   ***
(apply (lambda (a b c d e f z) z) '())
   ***

;;; testing dotted variables and closed applications with dotted variables.
(lambda args 1)
   ---
((lambda args 2))
   2
((lambda args args))
   ()
((lambda args args) 1)
   (1)
((lambda args args) 1 2)
   (1 2)
((lambda args args) 1 2 3)
   (1 2 3)
((lambda args (car args)) 1 2 3)
   1

((lambda (a . b) b) 1 2 3)
   (2 3)
((lambda (a . b) b) 1)
   ()
((lambda (a . b) b))
   ***

;;; testing the conversion of an application to a closed application
;;; with dotted variables.
((lambda a (cons a a)))
   (())
((lambda a (cons a a)) 1)
   ((1) 1)
((lambda a (cons a a)) 2 1)
   ((2 1) 2 1)
((lambda (x . a) (cons a a)))
   ***
((lambda (x . a) (cons a a)) 1)
   (())
((lambda (x . a) (cons a a)) 2 1)
   ((1) 1)
((lambda (x . a) (cons a a)) 3 2 1)
   ((2 1) 2 1)
((lambda (x y . a) (cons a a)))
   ***
((lambda (x y . a) (cons a a)) 1)
   ***
((lambda (x y . a) (cons a a)) 2 1)
   (())
((lambda (x y . a) (cons a a)) 3 2 1)
   ((1) 1)
((lambda (x y . a) (cons a a)) 4 3 2 1)
   ((2 1) 2 1)
((lambda (x y z . a) (cons a a)))
   ***
((lambda (x y z . a) (cons a a)) 1)
   ***
((lambda (x y z . a) (cons a a)) 2 1)
   ***
((lambda (x y z . a) (cons a a)) 3 2 1)
   (())
((lambda (x y z . a) (cons a a)) 4 3 2 1)
   ((1) 1)
((lambda (x y z . a) (cons a a)) 5 4 3 2 1)
   ((2 1) 2 1)
((lambda (x y z . a) (cons a a)) 6 5 4 3 2 1)
   ((3 2 1) 3 2 1)
((lambda (x y z u . a) (cons a a)))
   ***
((lambda (x y z u . a) (cons a a)) 1)
   ***
((lambda (x y z u . a) (cons a a)) 2 1)
   ***
((lambda (x y z u . a) (cons a a)) 3 2 1)
   ***
((lambda (x y z u . a) (cons a a)) 4 3 2 1)
   (())
((lambda (x y z u . a) (cons a a)) 5 4 3 2 1)
   ((1) 1)
((lambda (x y z u . a) (cons a a)) 6 5 4 3 2 1)
   ((2 1) 2 1)
((lambda (x y z u . a) (cons a a)) 7 6 5 4 3 2 1)
   ((3 2 1) 3 2 1)
((lambda (x y z u v . a) (cons a a)))
   ***
((lambda (x y z u v . a) (cons a a)) 1)
   ***
((lambda (x y z u v . a) (cons a a)) 2 1)
   ***
((lambda (x y z u v . a) (cons a a)) 3 2 1)
   ***
((lambda (x y z u v . a) (cons a a)) 4 3 2 1)
   ***
((lambda (x y z u v . a) (cons a a)) 5 4 3 2 1)
   (())
((lambda (x y z u v . a) (cons a a)) 6 5 4 3 2 1)
   ((1) 1)
((lambda (x y z u v . a) (cons a a)) 7 6 5 4 3 2 1)
   ((2 1) 2 1)
((lambda (x y z u v . a) (cons a a)) 8 7 6 5 4 3 2 1)
   ((3 2 1) 3 2 1)


((lambda z (cons 'z z)))
(z)
((lambda z (cons 'z z)) 1)
(z 1)
((lambda z (cons 'z z)) 1 2 3)
(z 1 2 3)
((lambda (x . z) (cons x z)) 1)
(1)
((lambda (x . z) (cons x z)) 1 2 3)
(1 2 3)
((lambda (x y . z) (cons x (cons y (cons z '())))) 1 2)
(1 2 ())
((lambda (x y . z) (cons x (cons y (cons z '())))) 1 2 3 4 5)
(1 2 (3 4 5))

;;; testing set! on dotted variable
((lambda args (begin (set! args (cdr args))
                     (car args) ))
 1 2 3 4 )
   2
(((lambda args (begin (set! args (cdr args)) 
                      (lambda (x) (cons x args)) ))
  1 2 3 4 )
 0 )
   (0 2 3 4)

(apply (lambda z (car z)) '(1 2 3 4 5 6 7))
   1
(apply (lambda z (car z)) 1 '(2 3 4 5 6 7))
   1
(apply (lambda z (car z)) 1 2 '(3 4 5 6 7))
   1
(apply (lambda z (car z)) 1 2 3 '(4 5 6 7))
   1
(apply (lambda z (car z)) 1 2 3 4 '(5 6 7))
   1
(apply (lambda z (car z)) 1 2 3 4 5 '(6 7))
   1
(apply (lambda (a . z) z) '())
   ***
(apply (lambda (a . z) (car z)) '(1 2 3 4 5 6 7))
   2
(apply (lambda (a . z) (car z)) 1 '(2 3 4 5 6 7))
   2
(apply (lambda (a . z) (car z)) 1 2 '(3 4 5 6 7))
   2
(apply (lambda (a . z) (car z)) 1 2 3 '(4 5 6 7))
   2
(apply (lambda (a . z) (car z)) 1 2 3 4 '(5 6 7))
   2
(apply (lambda (a . z) (car z)) 1 2 3 4 5 '(6 7))
   2
(apply (lambda (a b . z) z) '())
   ***
(apply (lambda (a b . z) z) '(1))
   ***
(apply (lambda (a b . z) (car z)) '(1 2 3 4 5 6 7))
   3
(apply (lambda (a b . z) (car z)) 1 '(2 3 4 5 6 7))
   3
(apply (lambda (a b . z) (car z)) 1 2 '(3 4 5 6 7))
   3
(apply (lambda (a b . z) (car z)) 1 2 3 '(4 5 6 7))
   3
(apply (lambda (a b . z) (car z)) 1 2 3 4 '(5 6 7))
   3
(apply (lambda (a b . z) (car z)) 1 2 3 4 5 '(6 7))
   3
(apply (lambda (a b c . z) z) '())
   ***
(apply (lambda (a b c . z) z) '(1))
   ***
(apply (lambda (a b c . z) z) '(1 2))
   ***
(apply (lambda (a b c . z) (car z)) '(1 2 3 4 5 6 7))
   4
(apply (lambda (a b c . z) (car z)) 1 '(2 3 4 5 6 7))
   4
(apply (lambda (a b c . z) (car z)) 1 2 '(3 4 5 6 7))
   4
(apply (lambda (a b c . z) (car z)) 1 2 3 '(4 5 6 7))
   4
(apply (lambda (a b c . z) (car z)) 1 2 3 4 '(5 6 7))
   4
(apply (lambda (a b c . z) (car z)) 1 2 3 4 5 '(6 7))
   4
(apply (lambda (a b c d . z) z) '())
   ***
(apply (lambda (a b c d . z) z) '(1))
   ***
(apply (lambda (a b c d . z) z) '(1 2))
   ***
(apply (lambda (a b c d . z) z) '(1 2 3))
   ***
(apply (lambda (a b c d . z) z) '(1 2 3 4))
   ()
(apply (lambda (a b c d . z) z) '(1 2 3 4 5))
   (5)
(apply (lambda (a b c d . z) (car z)) '(1 2 3 4 5 6 7))
   5
(apply (lambda (a b c d . z) (car z)) 1 '(2 3 4 5 6 7))
   5
(apply (lambda (a b c d . z) (car z)) 1 2 '(3 4 5 6 7))
   5
(apply (lambda (a b c d . z) (car z)) 1 2 3 '(4 5 6 7))
   5
(apply (lambda (a b c d . z) (car z)) 1 2 3 4 '(5 6 7))
   5
(apply (lambda (a b c d . z) (car z)) 1 2 3 4 5 '(6 7))
   5
(apply (lambda (a b c d e . z) z) '())
   ***
(apply (lambda (a b c d e . z) z) '(1))
   ***
(apply (lambda (a b c d e . z) z) '(1 2))
   ***
(apply (lambda (a b c d e . z) z) '(1 2 3))
   ***
(apply (lambda (a b c d e . z) z) '(1 2 3 4))
   ***
(apply (lambda (a b c d e . z) (car z)) '(1 2 3 4 5 6 7))
   6
(apply (lambda (a b c d e . z) (car z)) 1 '(2 3 4 5 6 7))
   6
(apply (lambda (a b c d e . z) (car z)) 1 2 '(3 4 5 6 7))
   6
(apply (lambda (a b c d e . z) (car z)) 1 2 3 '(4 5 6 7))
   6
(apply (lambda (a b c d e . z) (car z)) 1 2 3 4 '(5 6 7))
   6
(apply (lambda (a b c d e . z) (car z)) 1 2 3 4 5 '(6 7))
   6
(apply (lambda (a b c d e f . z) z) '())
   ***
(apply (lambda (a b c d e f . z) z) '(1))
   ***
(apply (lambda (a b c d e f . z) z) '(1 2))
   ***
(apply (lambda (a b c d e f . z) z) '(1 2 3))
   ***
(apply (lambda (a b c d e f . z) z) '(1 2 3 4))
   ***
(apply (lambda (a b c d e f . z) z) '(1 2 3 4 5))
   ***
(apply (lambda (a b c d e f . z) (car z)) '(1 2 3 4 5 6 7))
   7
(apply (lambda (a b c d e f . z) (car z)) 1 '(2 3 4 5 6 7))
   7
(apply (lambda (a b c d e f . z) (car z)) 1 2 '(3 4 5 6 7))
   7
(apply (lambda (a b c d e f . z) (car z)) 1 2 3 '(4 5 6 7))
   7
(apply (lambda (a b c d e f . z) (car z)) 1 2 3 4 '(5 6 7))
   7
(apply (lambda (a b c d e f . z) (car z)) 1 2 3 4 5 '(6 7))
   7

;;; call/cc
call/cc
   ---
(call/cc (lambda (k) 'a))
   a
(call/cc (lambda (k) (k 'a)))
   a
(call/cc (lambda (k) (cons (k 'a) 1)))
   a
(call/cc call/cc)
   ---
;;; test that lexical contexts are preserved through continuations.
;;; The expression is now insensitive to evaluation order.
((lambda (foo)
   ((lambda (v1)
      (cons v1 '()) )
    (foo (call/cc (lambda (k)
                    (k 'a) ))) ) )
 (lambda (x) x) )
   (a)

((lambda (foo)
   (foo (call/cc (lambda (k)         ; store A and return Z
                   (k 'a) ))) )
 ((lambda (z)
    (lambda (x)
      ((lambda (y) (begin (set! z x) y))
       z ) ) )
  'z ) )
   z

((lambda (foo)
   (begin 
     (set! foo ((lambda (z)
                  (lambda (x)
                    ((lambda (y) (begin (set! z x) y))
                     z ) ) )
                'z ))
     (foo (call/cc (lambda (k)         ; store A and return Z
                     (k 'a) ))) ) )
 'foo )
   z

((lambda (foo)
   (begin                               ; a letrec by hand
     (set! foo ((lambda (z)
                  (lambda (x)
                    ((lambda (y) (begin (set! z x) y))
                     z ) ) )
                'z ))
     ((lambda (v1)
        (cons v1 '()) )
      (foo (call/cc (lambda (k)         ; store A and return Z
                      (k 'a) ))) ) ) )
 'foo )
   (z)

;;; exercize some errors when invoking continuations
(call/cc (lambda (k) (k)))
   ***
(call/cc (lambda (k) (k 1 2)))
   ***
((call/cc (lambda (k) k)) 1 2 3)
   ***
(call/cc (lambda (k) (cons 1 (apply k 2 '()))))
   2
(call/cc (lambda (k) (cons 1 (apply k '(3)))))
   3
;;; variable arity on continuations
(call/cc (lambda (k) (cons 1 (apply k 1 2 '()))))
   ***
(call/cc (lambda (k) (cons 1 (apply k '()))))
   ***
;;; Using nary functions
(call/cc list)
   ---
(call/cc (lambda k* (cons 1 ((car k*) 2))))
   2


"*** *** *** *** *** *** * Attention * *** *** *** ** ***
The following test forces a continuation to return multiply."
   ---
;;; Testing let coalescence with respect to call/cc.
((lambda (the-k return)
   ((lambda (foo)
      ((lambda (f1)
         ((lambda (r2)
            ((lambda (f3)
               ((lambda (r4)
                  ((lambda (r5)
                     (list r2 r4 r5) )
                   (f3 5) ) )              ; -> (4.5)
                (f1 5) ) )                 ; -> (3.5)  but (4.5) if +
             (call/cc (lambda (exit)       ;                        |
                        (set! return exit) ;                        |
                        (the-k 4) )) ) )   ; -> \y.cons(4,y)        |
          (f1 5) ) )                       ; -> (3.5)               |
       (foo 3) ) )                         ; -> \y,cons(3,y)        |
    (lambda (v)                            ;                        V
      ((lambda (x)                         ; <-----frame for x is prealloc'd
         (return (lambda (y) (cons x y))) )
       (call/cc (lambda (k) (set! the-k k) v)) ) ) ) )
 0 (lambda (x) x) )
   ((3 . 5) (3 . 5) (4 . 5))

"*** *** *** *** *** *** * Attention * *** *** *** ** ***
The following tests use continuations out of their dynamic extent."
   ---
((lambda (a)
   (begin (set! a (call/cc (lambda (k) k)))
          (a (lambda (x) 1)) ) )
 2 )
   1
((lambda (a b)
   (begin (set! a (call/cc (lambda (k) k)))
          (begin (set! b (+ 1 b))
                 (a (lambda (x) b)) ) ) )
 3 0 )
   2
((lambda (foo)
   (begin ; a letrec by hand
     (set! foo ((lambda (z)
                  (lambda (x)
                    ((lambda (y) (begin (set! z x) y))
                     z ) ) )
                'z ))
     ((lambda (v1)
        (cons v1
              ((lambda (v2)
                 (cons v2
                       ((lambda (v3)
                          (cons v3
                                ((lambda (v4)
                                   (cons v4 
                                         (cons (foo 'f) 
                                               '() )) )
                                 ((lambda (x)
                                    (if (symbol? x) 
                                        'ee 
                                        (x 'e) ) )
                                  ; store D, return the continuation
                                  (foo 'd) ) ) ) )
                        (foo (call/cc (lambda (k) 
                                        ; store continuation, return B
                                        (cons 2 (k k)) ))) ) ) )
               (foo (call/cc (lambda (k) ; store B, return A
                               (cons 1 (k 'b)) ))) ) ) )
      (foo (call/cc (lambda (k)         ; store A and return Z
                      (k 'a) ))) ) ) )
 'foo )
   (z a d ee d)

((lambda (foo)
   (begin
     (set! foo ((lambda (z)
                  (lambda (x u);; <-- two variables now
                    ((lambda (y) (begin (set! z x) y))
                     z ) ) )
                'z ))
     ((lambda (v1)
        (cons v1
              ((lambda (v2)
                 (cons v2
                       ((lambda (v3)
                          (cons v3
                                ((lambda (v4)
                                   (cons v4 (cons (foo 'f 5) '())) )
                                 ((lambda (x)
                                    (if (symbol? x) 'ee (x 'e)) )
                                  (foo 'd 4) ) ) ) )
                        (foo (call/cc (lambda (k) 
                                        ; store continuation, return B
                                        (cons 2 (k k)) ))
                             3 ) ) ) )
               (foo (call/cc (lambda (k) ; store B, return A
                               (cons 1 (k 'b)) ))
                    2 ) ) ) )
      (foo (call/cc (lambda (k)         ; store A and return Z
                      (k 'a) ))
           1 ) ) ) )
 'foo )
   (z a d ee d)
;;; From an idea of D. Friedman reported to me by O. Danvy: 
;;; How can be used (call/cc list) ?
((lambda (r)
   ((car r) (list pair?)) )
 (call/cc list) )
   #t
;;; The answer of Luc Moreau
((lambda (a)
   ((car ((car a) (list (lambda (x) x)))) 3) )
 (call/cc list) )
   3

;;; check that activation frames are not shared (variable a).
((lambda (k f)
  (begin 
    (set! f ((lambda (r) (cons r f))
             ((lambda (a) (lambda () a))
              (call/cc (lambda (nk) 
                         (begin (set! k nk) (nk 1)) )) ) ))
    (if (eq? (cdr f) '()) 
        (k 2) 
        (list ((car f)) ((car (cdr f)))) ) ) )
 'wait '() )
   (2 1)
((lambda (k f g)
  (begin 
    (set! f ((lambda (r) (cons r f))
             (g (call/cc (lambda (nk) 
                           (begin (set! k nk) (nk 1)) )) ) ))
    (if (eq? (cdr f) '()) 
        (k 2) 
        (list ((car f)) ((car (cdr f)))) ) ) )
 'wait '() (lambda (a) (lambda () a)) )
   (2 1)

;;; Try now to test a little more the syntactical side
;;; implicit progn in the body of a lambda
;((lambda (a) 1 2 3) 4)
;   3
;;; should appear in another test suite.

;;; end of scheme.tst
