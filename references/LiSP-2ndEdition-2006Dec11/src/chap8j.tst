;;; $Id: chap8j.tst,v 4.0 1995/07/10 06:52:12 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; testing reflisp

33

nil

(car (cons 1 2))

(lambda (x) x)

((lambda (x y) (cons x y))
 3 4 )

((lambda (x) (set! x (cons x x))
             x )
 33 )

(car (quote (a b)))

(if 1 2 3)

(if #f 3 4)

;;; flambda
((flambda (r) r))

((flambda (r) 3))

((flambda (r x) (cons r x)) (+ 1 2))

;;; defining usual special forms
(set! global-env 
      (enrich global-env 'begin 'the-environment) )

(set! begin 
      (flambda (r . forms)
         (eprogn forms r) ) )

(set! the-environment
      (flambda (r) r) )

;;; and using them
(begin 1 (display 2) 3)

(the-environment)

((lambda (fact)
   (set! fact (lambda (n) 
                (if (= n 0) 1 (* n (fact (- n 1)))) ))
   (fact 4) )
 'fact )

;;; Reflective library
(eval '3 (the-environment))

(eval '(cons 1 2) (the-environment))

(evlis '(1 (+ 2 2)) (the-environment))

((reference 'car (the-environment)) (cons 1 2))

(eprogn '(1 (display 2) 3) (the-environment))

;;; New special form
(set! global-env (enrich global-env 'when))

(set! when 
      (flambda (r condition . body)
        (if (eval condition r) (eprogn body r) #f) ) )

(list (when 1 2 3) (when #f 5 6))

;;; The define special form

(set! global-env (enrich global-env 'define))

(set! define (flambda (r name form)
               (if (variable-defined? name r)
                   (set-variable-value! name r (eval form r))
                   ((lambda (rr)
                      (set! global-env rr)
                      (set-variable-value! name rr (eval form rr)) )
                    (enrich r name) ) ) ))

(define unless
  (flambda (r condition . body)
    (if (eval condition r)
        #f
        (eprogn body r) ) ) )

(unless 1 2 3)

(unless #f (display 34))

;;; New level
(make-toplevel "?1? " "=1= ")

(* 3 4)

(make-toplevel "?2?" "=2=")

(display "Attention, this error will abort level 2.")

(cons) ; back to level 1

(display "Attention, this error will not abort level 1.")

(monitor (lambda (c b) 
           (display b)
           b )
  (car) )

(display "Attention, go back to base level.")

(exit (+ 1 2))

;;; bound to the last value
it

;;; Changing an existing special form
;;; There is a problem if the variable is undefined before.
(set! set! 
      (flambda (r name form)
        ((lambda (new-value) 
           (if (variable-defined? name r)
               ((lambda (old-value)
                  (set-variable-value! name r new-value)
                  old-value )
                (variable-value name r) )
               (if (variable-defined? name global-env)
                   ((lambda (old-value)
                      (set-variable-value! name global-env new-value)
                      old-value )
                    (variable-value name global-env) )
                   (error "No such variable" name) ) ))
         (eval form r) ) ) )

(flambda? (set! when (lambda (x . others) x)))

;;; Can interpret itself. Since global env is shared, begin is also
;;; defined. The trick is that flambda-tag is the same between the two
;;; levels of the interpreter.
(display "Attention, self-interpretation.")

(eval reflisp-code global-env)

(* 3 4)

(if global-env (* 5 3) 0)

(begin 1 2 3)

(display "Attention, back to fundamental level.")

(exit 'end)

;;; end of chap8j.tst
