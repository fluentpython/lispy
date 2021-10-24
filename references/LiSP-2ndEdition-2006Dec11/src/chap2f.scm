;;; $Id: chap2f.scm,v 4.1 2006/11/09 18:58:57 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Scheme + dynamic variables without special forms.

(define (dd.evaluate e env denv)
  (if (atom? e) 
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e)
                 (boolean? e) (vector? e) )
             e )
            (else (wrong "Cannot evaluate" e)) )
      (case (car e)
        ((quote) (cadr e))
        ((if) (if (dd.evaluate (cadr e) env denv)
                  (dd.evaluate (caddr e) env denv)
                  (dd.evaluate (cadddr e) env denv) ))
        ((begin) (dd.eprogn (cdr e) env denv))
        ((set!) (update! (cadr e) 
                         env 
                         (dd.evaluate (caddr e) env denv)))
        ((lambda) (dd.make-function (cadr e) (cddr e) env))
        (else (invoke (dd.evaluate (car e) env denv)
                      (dd.evlis (cdr e) env denv)
                      denv )) ) ) )

(define (dd.make-function variables body env)
  (lambda (values denv)
    (dd.eprogn body (extend env variables values) denv) ) )

(define (dd.evlis e* env denv)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (cons (dd.evaluate (car e*) env denv)
                (dd.evlis (cdr e*) env denv) )
          (list (dd.evaluate (car e*) env denv)) )
      '() ) )

(define (dd.eprogn e* env denv)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (begin (dd.evaluate (car e*) env denv)
                 (dd.eprogn (cdr e*) env denv) )
          (dd.evaluate (car e*) env denv) )
      empty-begin ) )  

(define (lookup id env)
  (if (pair? env)
      (if (eq? (caar env) id)
          (cdar env)
          (lookup id (cdr env)) )
      (wrong "No such binding" id) ) )

(define (update! id env value)
  (if (pair? env)
      (if (eq? (caar env) id)
          (begin (set-cdr! (car env) value)
                 value )
          (update! id (cdr env) value) )
      (wrong "No such binding" id) ) ) 

(define (extend env variables values)
  (cond ((pair? variables)
         (if (pair? values)
             (cons (cons (car variables) (car values))
                   (extend env (cdr variables) (cdr values)) )
             (wrong "Too less values") ) )
        ((null? variables)
             (if (null? values)
                 env 
                 (wrong "Too much values") ) )
        ((symbol? variables) (cons (cons variables values) env)) ) ) 

(define (invoke fn args denv)
  (if (procedure? fn) 
      (fn args denv)
      (wrong "Not a function" fn) ) ) 

(define the-false-value #f)

;;; Initial environment

(define env.global '())
(define denv.global '())

(define-syntax definitial 
  (syntax-rules ()
    ((definitial name)
     (begin (set! env.global (cons (cons 'name 'void) env.global))
            'name ) )
    ((definitial name value)
     (begin (set! env.global (cons (cons 'name value) env.global))
            'name ) ) ) )

(define-syntax defprimitive 
  (syntax-rules ()
   ((defprimitive name value arity)
    (definitial name
      (lambda (values denv)
        (if (= arity (length values))
            (apply value values)
            (wrong "Incorrect arity" (list 'name values)) ) ) ) ) ) )

(define-syntax defpredicate 
  (syntax-rules ()
    ((defpredicate name value arity)
     (defprimitive name
       (lambda values (or (apply value values) the-false-value))
       arity ) ) ) )

(definitial t #t)
(definitial f the-false-value)
(definitial nil '())

(definitial x)
(definitial y)
(definitial z)
(definitial a)
(definitial b)
(definitial c)
(definitial foo)
(definitial bar)
(definitial hux)
(definitial fib)
(definitial fact)
(definitial visit)
(definitial length)
(definitial primes)

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defpredicate pair? pair? 1)
(defpredicate symbol? symbol? 1)
(defprimitive eq? eq? 2)
(defpredicate eq? eq? 2)
(defprimitive set-car! set-car! 2)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defpredicate = = 2)
(defprimitive < < 2) 
(defpredicate < < 2)
(defpredicate > > 2)
(defprimitive * * 2)
(defpredicate <= <= 2)
(defpredicate >= >= 2)
(defprimitive remainder remainder 2)
(defprimitive display display 1)
(defprimitive equal? equal? 2)

(definitial error 'no-error-here)

(defprimitive call/cc 
  (lambda (f) 
    (call/cc (lambda (g) 
               (invoke f
                       (lambda (values denv)
                         (if (= (length values) 1)
                             (g (car values))
                             (wrong "Incorrect arity" g) ) )
                       denv ) )) )
  1 )   

(definitial apply 
  (lambda (values denv)
    (if (>= (length values) 2)
        (let ((f (car values))
              (args (let flat ((args (cdr values)))
                      (if (null? (cdr args))
                          (car args)
                          (cons (car args) (flat (cdr args))) ) )) )
        (invoke f args denv) )
        (wrong "Incorrect arity" 'apply) ) ) )

(definitial list 
  (lambda (values denv) values) )

(definitial bind/de
  (lambda (values denv)
    (if (= 3 (length values))
        (let ((tag (car values))
              (value (cadr values))
              (thunk (caddr values)) )
          (invoke thunk '() (extend denv (list tag) (list value))) )
        (wrong "Incorrect arity" 'bind/de) ) ) )

(definitial assoc/de
  (lambda (values current.denv)
    (if (= 2 (length values))
        (let ((tag     (car values))
              (default (cadr values)) )
          (let look ((denv current.denv))
            (if (pair? denv)
                (if (eqv? tag (caar denv))
                    (cdar denv)
                    (look (cdr denv)) )
                (invoke default (list tag) current.denv) ) ) )
        (wrong "Incorrect arity" 'assoc/de) ) ) )

(definitial new-assoc/de
  (lambda (values current.denv)
    (if (= 3 (length values))
        (let ((tag        (car values))
              (default    (cadr values))
              (comparator (caddr values)) )
          (let look ((denv current.denv))
            (if (pair? denv)
                (if (eq? the-false-value 
                         (invoke comparator (list tag (caar denv)) 
                                            current.denv ) )
                    (look (cdr denv))
                    (cdar denv) )
                (invoke default (list tag) current.denv) ) ) )
        (wrong "Incorrect arity" 'assoc/de) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;;  Tests

(define (scheme2f)
  (interpreter
   "Scheme? "
   "Scheme= "
   #t
   (lambda (read print error)
     (define (skip-read)
       (let ((e (read)))
         (if (member e *tests-to-skip*)
             (begin (read)              ; skip the associated result
                    (skip-read) )
             e ) ) )
     (set! wrong error)
     (lambda ()
       (print (dd.evaluate (skip-read) env.global denv.global)) ) ) ) )

(define (test-scheme2f file)
  (suite-test
   file
   "Scheme? "
   "Scheme= "
   #t
   (lambda (read check error)
     (define (skip-read)
       (let ((e (read)))
         (if (member e *tests-to-skip*)
             (begin (read)              ; skip the associated result
                    (skip-read) )
             e ) ) )
     (set! wrong error)
     (lambda ()
       (check (dd.evaluate (skip-read) env.global denv.global)) ) )
   (lambda (expected obtained)
     (or (equal? expected obtained)
         (and (eq? obtained the-false-value) (eq? expected #f)) ) ) ) )

(define *tests-to-skip*
  '(
    ) )


;;; end of chap2f.scm
