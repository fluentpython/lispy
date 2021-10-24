;;; $Id: chap9c.tst,v 4.0 1995/07/10 06:52:19 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

(when #t 1)
   1
(when #t 1 2 3)
   3
(when #f 1 2)
   #f
(when (pair? (cons 'a 'b)) (list 'c))
   (c)
(when (pair? 'foo) 'bar)
   #f

(let ((x 22)) (- x 1))
   21
(let ((x 22)(y 3)) (+ x y) (- x y))
   19
(let () 12)
   12

; test backquote
`(quote a)
   (quote a)
(let ((x 11)) `(quote ,x))
   (quote 11)
(let ((x '(111 22)))
  `(quote (,@x a b)) )
   (quote (111 22 a b))
(let ((x '(111 22)))
  `(quote (,@x ,@x)) )
   (quote (111 22 111 22))
(let ((x '(111 22)))
  `(quote (,@x . ,x)) )
   (quote (111 22 111 22))

;;; eval-in-abbreviation-world
(eval-in-abbreviation-world #t)
   #t
(eval-in-abbreviation-world
 (display "!!!!!!!!!!!!!!!!!!!!!!!!")
 #f )
   #f ; and displays a lot of !
; the result of eval-in-abbreviation-world replaces that form
(eval-in-abbreviation-world
 '(quote foo) )
   foo
; let is present in macro world:
(eval-in-abbreviation-world
 (let ((x '(quote bar)))
   x ) )
   bar
; define a new global variable in macro world
(list (eval-in-abbreviation-world
        (set! foo 22)
        (display foo)
        (list 'quote foo) ))
   (22)
;;; Make sure the expansion order so use begin (instead of list) below:
(begin (eval-in-abbreviation-world
         (set! foo 34)
         '(quote nothing) )
       (eval-in-abbreviation-world
         (list 'quote foo) ) )
   34
; create a variable in macro macro world 
(begin (eval-in-abbreviation-world
         (eval-in-abbreviation-world
           (set! bar 56)
           '(quote nothing2) )
         '(quote nothing1) )
       (eval-in-abbreviation-world
         (eval-in-abbreviation-world
           bar ) ) )
   56

;;; define-abbreviation
(begin
  (define-abbreviation (foo x) 
    (list 'list x x) )
  (foo 33) )
   (33 33)
; tests are done sequentially in the same store so foo is still
; defined and present.
(foo (display "!!!!!"))
   ---   ; and displays twice !!!!!
; redefine global macro foo
(define-abbreviation (foo x y)
  (list 'list y x) )
   ---
(foo (+ 1 2) (- 6 4))
   (2 3)
;; check that a global macro is shadowed by a lexical binding
((lambda (foo) (foo (+ 1 2) (- 6 4)))
 (lambda (x y) 33) )
   33
;; dotted variable macro
(define-abbreviation (foo x . y)
  (cons 'begin (cons x y)) )
   ---
(foo 3 4 5)
   5
(foo 3)
   3
(foo)
   *** ; missing parameters for macro foo

;;; macroexpanded exp can use macros
(define-abbreviation (mac1 x)
  (list 'foo x (list '+ x x)) )
   ---
(mac1 (+ 3 4))
   14

;;; a macro can generate a macro
(define-abbreviation (mac2 call body)
  (list 'define-abbreviation call body) )
   ---
(begin (mac2 (mac3 xx yy) (list 'list yy 33 xx))
       (mac3 (* 8 7) 3) )
   (3 33 56)

;;; define a macro BAR in the macro world
(eval-in-abbreviation-world
 (define-abbreviation (bar x)
   (list 'list 'list 2 x 2) ) )
   ---
; defines a macro that uses BAR when expanding
(define-abbreviation (hux x)
  (list 'list 1 (bar x) 1) )
   ---
; check BAR is really expanded
(hux (+ 2 3))
   (1 (2 5 2) 1)
; check BAR is not a 1st level macro
(bar (+ 4 5))
   ***
; check macros are hyperstatic, so hux does not depend on a redefinition of bar
(begin (eval-in-abbreviation-world
        (define-abbreviation (bar x)
          (list 'list 'list 4 x 4) ) )
       (hux (+ 2 3)) )
   (1 (2 5 2) 1)
; redefine hux to take notice of the new bar
(begin (define-abbreviation (hux x)
         (list list 1 (bar x) 1) )
       (hux (+ 2 3)) )
   (1 (4 5 4) 1)


;;; Local macros
(define-abbreviation (hux x)
  (list 'list 1 x 1) )
   ---
(list (hux (+ 2 3))
      (let-abbreviation (( (hux x y) (list 'list 3 y x 3)) )
          (list (hux (+ 5 6) (+ 7 8))) )
      (hux (* 4 5)) )
   ( (1 5 1) ( (3 15 11 3) ) (1 20 1) )
(hux (+ 3 4))
   (1 7 1)
; check that a local macro can be shadowed a lexical variable
(list (hux (+ 2 3))
      (let-abbreviation (( (hux x y) (list 'list 3 y x 3)) )
          ((lambda (hux)
             (list (hux (+ 5 6) (+ 7 8))) )
           (lambda (x y) (list 'hux y x)) ) )
      (hux (* 4 5)) )
   ( (1 5 1) ( (hux 15 11) ) (1 20 1) )

;;; availability of eval
(eval '(* 2 3))
   6
; eval is at toplevel 
(let ((cons 3))
  ((eval 'cons) 4 5) )
   (4 . 5)
; another eval exists in macroworld
(eval-in-abbreviation-world
   (eval '(* 2 3)) )
   6
(eval-in-abbreviation-world
    (list 'quote (let ((cons 3))
                   ((eval 'cons) 4 5) )) )
   (4 . 5)
; foo exists in macro-world
(eval-in-abbreviation-world
 (list 'quote (eval 'foo)) )
   34
; eval knows essential syntaxes
(eval '(let ((x (+ 3 4))) (* x x)))
   49

;;; prepare is not available at toplevel
;prepare
;   ***
;;;; but can be used in macro world
;(eval-in-abbreviation-world
; (prepare '(cons 1 2)) )
;   (1 . 2)
;;;; predefined macros are still visible
;(eval-in-abbreviation-world
; (prepare '(let ((x (* 3 4))) (+ x x))) )
;   24
;; prepare can be composed
;(eval-in-abbreviation-world
; (list 'car (prepare '(let ((x (* 3 4))) (cons x x)))) )
;   12
;; prepare sees global macros
;(eval-in-abbreviation-world
; (list 'list (prepare '(hux (* 5 6)))) )
;   ((1 30 1))
;;;; prepare sees local macros
;(let-abbreviation (( (hux x y) (list 'list 3 y x 3)) )
;   (eval-in-abbreviation-world
;     (prepare '(hux (* 3 4) 5)) ) )
;   (3 5 12 3)
;; Weird case, there's a mix up of preparation made at different levels.
;(eval-in-abbreviation-world
; (eval-in-abbreviation-world
;  (prepare '(let ((x 22)) x)) ) )
;   ???
;
;
;;;; availability of expand
;(expand '(* 2 3))
;   (* 2 3)
;(expand '(hux (* 3 4)))
;   (list 1 (* 3 4) 1)
;; no macro bar at this level
;(expand '(bar (* 4 5)))
;   (bar (* 4 5))
;; another expand in macroworld
;(eval-in-abbreviation-world
; (list 'quote (expand '(bar (+ 4 5)))) )
;   (list 2 (+ 4 5) 2)
;; no macro hux in macroworld
;(eval-in-abbreviation-world
; (list 'quote (expand '(hux (* 8 9)))) )
;   (hux (* 8 9))
;; expand is toplevel
;(let-abbreviation (( (hux x y) (list 'list 3 y x 3)) )
;     (list (expand '(hux (+ 5 6) (+ 7 8)))) )
;   (hux (+ 5 6) (+ 7 8))

;;; Hygien and with-aliases 
; on a predefined global variable
(with-aliases ((x car))
  (eval-in-abbreviation-world
   (display x)(newline)
   (list x ''(a b)) ) )
  a
(with-aliases ((x car))
  (let ((car cdr))
    (eval-in-abbreviation-world
     (list x ''(a b)) ) ) )
  a
(with-aliases ((x car))
  (let ((car cdr))
    (eval-in-abbreviation-world
     (list x '(car '(b a))) ) ) )
  a
(with-aliases ((x car))
  (let ((car cdr)(x 33))
    (eval-in-abbreviation-world
     (list x '(car '(b a))) ) ) )
  a
; aliases are visible not only from eval-in-abbreviation-world but
; also from let-abbreviation
(with-aliases ((x car))
  (let-abbreviation ( ( (werk y) (list x (list 'cons y 33)) ) )
    (list (werk (* 2 3)) (car '(a b))) ) )
   (6 a)
(with-aliases ((x car))
  (let ((car cdr)(x 45))
    (let-abbreviation ( ( (werk y) (list x (list 'cons y 33)) ) )
      (list (werk (* 2 3)) (car '(a b))) ) ) )
   (6 (b))

; on a global mutable variable (separately created)
(set! a 312)
   ---
(with-aliases ((x a))
   (eval-in-abbreviation-world
     x ) )
   312
(with-aliases ((x a))
  (let ((a 11)(x 33))
    (eval-in-abbreviation-world
       (list 'cons x 'a) ) ) )
   (312 . 11)
(with-aliases ((x a))
  (let ((a 11)(x 33))
    (let-abbreviation ( ( (werk y) (list 'cons x (list 'cons y 33)) ) )
      (list (werk (* 2 3)) (car '(a b))) ) ) )
    ((312 6 . 33) A)

; on a magic word
(with-aliases ((q quote)(s set!)(l lambda)(b begin)(i if))
  (eval-in-abbreviation-world
    (list q (* 22 3)) ) )
   66
(with-aliases ((q quote)(s set!)(l lambda)(b begin)(i if))
 (let ((q 1)(s 2)(l 3)(b 4)(i 5)
       (quote 11)(set! 22)(lambda 33)(begin 44)(if 55) )
   (eval-in-abbreviation-world
     (list q (* 22 3)) ) ) )
   66
(with-aliases ((q quote)(s set!)(l lambda)(b begin)(i if))
  (eval-in-abbreviation-world
    (list s 'x (* 22 3)) )
  x )
   66
(with-aliases ((q quote)(s set!)(l lambda)(b begin)(i if))
  (let ((q 1)(s 2)(l 3)(b 4)(i 5)
       (quote 11)(set! 22)(lambda 33)(begin 44)(if 55) )
     (eval-in-abbreviation-world
      (list s 'x (* 22 3)) ) )
  x )
   66
(with-aliases ((q quote)(s set!)(l lambda)(b begin)(i if))
  (eval-in-abbreviation-world
      (list b (list s 'x (* 22 3))
              'x ) ) )
   66
(with-aliases ((q quote)(s set!)(l lambda)(b begin)(i if))
  (let ((q 1)(s 2)(l 3)(b 4)(i 5)
       (quote 11)(set! 22)(lambda 33)(begin 44)(if 55) )
     (eval-in-abbreviation-world
       (list b (list s 'x (* 22 3))
              'x ) ) ) )
   66
(with-aliases ((q quote)(s set!)(l lambda)(b begin)(i if))
  (eval-in-abbreviation-world
    (list i #t 1 2) ) )
   1
(with-aliases ((q quote)(s set!)(l lambda)(b begin)(i if))
  (let ((q 1)(s 2)(l 3)(b 4)(i 5)
       (quote 11)(set! 22)(lambda 33)(begin 44)(if 55) )
     (eval-in-abbreviation-world
      (list i #t 1 2) ) ) )
   1
(with-aliases ((q quote)(s set!)(l lambda)(b begin)(i if))
  (eval-in-abbreviation-world
    (list i #f 1 2) ) )
   2
(with-aliases ((q quote)(s set!)(l lambda)(b begin)(i if))
  (let ((q 1)(s 2)(l 3)(b 4)(i 5)
       (quote 11)(set! 22)(lambda 33)(begin 44)(if 55) )
     (eval-in-abbreviation-world
      (list i #f 1 2) ) ) )
   2

; on a local variable
(let ((x 11))
  (with-aliases ((xx x))
    (eval-in-abbreviation-world
      xx ) ) )
   11
(let ((x 11))
  (with-aliases ((xx x))
    (let ((x 22))
      (eval-in-abbreviation-world
        xx ) ) ) )
   11

; big example
(let ((results '())
      (compose cons) )
  (with-aliases ((s set!) (results results) (compose compose))
    (let-abbreviation
     ( ( (push e) `(,s ,results (,compose ,e ,results)) ) 
       ( (done)   results ) )
     (let ((data '(1 (2 3 ((4)) . 5) (((6) 7) 8 . 9)))
           (foo  'wait) )
       (set! foo (lambda (l)
                   (if (atom? l)
                       (if (null? l) 'nothing (push l))
                       (begin (foo (car l))
                              (foo (cdr l)) ) ) ))
       (foo data)
       (done) ) )) )
   (9 8 7 6 5 4 3 2 1)

(with-aliases ((cc call/cc)(lam lambda) (ll let))
  (define-abbreviation (loop . body)
    (let ((loop (gensym)))
      `(,cc (,lam (exit) (,ll ,loop () ,@body (,loop)))) ) ) )
   ---
(let ((i 1))
  (loop (when (> i 30) (exit 33))
        (display i)
        (set! i (+ i 1)) ) )
   33
">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>other example from book"
   ---
; One cannot write since the bindings for cc, lam and ll belong to the
; macro macro world.
(define-abbreviation (loop . body)
  (with-aliases ((cc call/cc)(lam lambda) (ll let))
    (let ((loop (gensym)))
      `(,cc (,lam (exit) (,ll ,loop () ,@body (,loop)))) ) ) )
   ---
(let ((i 1))
  (loop (when (> i 30) (exit 33))
        (display i)
        (set! i (+ i 1)) ) )
   ***


;;; Macros can capture local bindings and still refer to them
(let ((x 3))
  (with-aliases ((xx x))
    (define-abbreviation (err1) xx) )
  (err1) )
   3
(begin (let ((x 3))
         (with-aliases ((xx x))
           (define-abbreviation (err1) xx) )
         x )
       (err1) )
   *** ; non longer visible variable: x
">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>other example from book"
   ---
(let ((count (let ((counter 0))
               (lambda () (set! counter (+ 1 counter)) counter) )))
  (with-aliases ((count count))
    (define-abbreviation (tick)
      `(,count) ) ) )
   ---
(let ((count 33))
  (tick) )
   *** ; count no more visible.
;;; simpler example
(let ((count 0))
  (with-aliases ((c count))
    (define-abbreviation (tick) c) )
  (tick) )
   0
(let ((count 1)(c 2)) 
  (tick) )
   ***

; a macro generating a macro within the scope of an alias
(let ((x 11))
  (with-aliases ((xx x))
    (let-abbreviation 
       (( (mac11 call definition . body)
          `(let-abbreviation 
                ((,call ,definition))
               . ,body ) ))
       (let ((x 22)(xx 33))
         (mac11 (mac12 y) `(list ,xx)   (cons (mac12 (* 3 4)) x)) ) ) ) )
 ((11) . 22)

;;; Exercice 1
(with-aliases ((+let let) (+begin begin) (+when when) (+not not))
  (define-abbreviation (repeat2 . parms)
    (let ((p    (list-ref parms 1))
          (q    (list-ref parms 3))
          (body (list-tail parms 5))
          (loop (gensym)) )
      `(,+let ,loop () 
            (,+when ,p (,+begin (,+when (,+not ,q) . ,body)
                                (,loop) )) ) ) ) )
   ---
(let ((i 0))
  (repeat2 :while (begin (set! i (+ i 1)) (< i 10))
           :unless (= 0 (modulo i 2))
           :do (display i)(display i) )
  i )
   10 ; and prints 1133...99

;;;; exercice 2
; take it easy
;(define-abbreviation (define name form)
;  `(set! ,name ,form) )
;   ---
; does not work here since the define form and the abbreviation do not
; share the same world.
;(define-abbreviation (define-immediate-abbreviation call . body)
;  (let ((name (gensym)))
;    `(begin (define ,name (lambda ,(cdr call) . ,body))
;            (define-abbreviation ,call (,name . ,(cdr call))) ) ) )
;   ---
;(define-immediate-abbreviation (flagada tsoin)
;  `(quote (,tsoin ,tsoin)) )
;   ---
;(flagada 33)
;   (33 33)

(define-abbreviation (define-alias newname oldname)
  `(define-abbreviation (,newname . parameters)
     (cons ',oldname parameters) ) )
 ---
(define-alias r2 repeat2)
   ---
(let ((i 0))
  (r2 :while (begin (set! i (+ i 1)) (< i 10))
      :unless (= 0 (modulo i 2))
      :do (display i)(display i) )
  i )
   10 ; and prints 1133...99


;;; end of chap9c.tst
