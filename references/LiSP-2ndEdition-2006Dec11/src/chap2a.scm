;;; $Id: chap2a.scm,v 4.2 2006/11/09 18:51:21 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Excerpts of chapter 2 (Lisp1, Lisp2)

(define (f.evaluate e env fenv)
  (if (atom? e) 
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e)
                 (boolean? e) (vector? e) )
             e )
            (else (wrong "Cannot evaluate" e)) )
      (case (car e)
        ((quote)  (cadr e))
        ((if)     (if (f.evaluate (cadr e) env fenv)
                      (f.evaluate (caddr e) env fenv)
                      (f.evaluate (cadddr e) env fenv) ))
        ((begin)  (f.eprogn (cdr e) env fenv))
        ((set!)   (update! (cadr e) 
                           env 
                           (f.evaluate (caddr e) env fenv) ))
        ((lambda) (f.make-function (cadr e) (cddr e) env fenv))
        (else     (evaluate-application (car e) 
                                        (f.evlis (cdr e) env fenv)
                                        env 
                                        fenv )) ) ) )

(define (f.evlis exps env fenv)
  (if (pair? exps)
      (cons (f.evaluate (car exps) env fenv)
            (f.evlis (cdr exps) env fenv) )
      '() ) )

(define (f.eprogn exps env fenv)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (f.evaluate (car exps) env fenv)
                 (f.eprogn (cdr exps) env fenv) )
          (f.evaluate (car exps) env fenv) )
      empty-begin ) )

(define (f.make-function variables body env fenv)
  (lambda (values)
    (f.eprogn body (extend env variables values) fenv) ) )

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

(define (invoke fn args)
  (if (procedure? fn) 
      (fn args)
      (wrong "Not a function" fn) ) ) 

(define (evaluate-application fn args env fenv)
  (cond ((symbol? fn)
         (invoke (lookup fn fenv) args) )
        ((and (pair? fn) (eq? (car fn) 'lambda))
         (f.eprogn (cddr fn)
                   (extend env (cadr fn) args)
                   fenv ) )
        (else (wrong "Incorrect functional term" fn)) ) )

;;; Variant where the functional term is evaluated.

(define (evaluate-application2 fn args env fenv)
  (cond ((symbol? fn)
         ((lookup fn fenv) args) )
        ((and (pair? fn) (eq? (car fn) 'lambda))
         (f.eprogn (cddr fn)
                   (extend env (cadr fn) args)
                   fenv ) )
        (else (evaluate-application2                 ;\modified
               (f.evaluate fn env fenv) args env fenv )) ) )

(define the-false-value #f)

;;; Initial environment

(define env.global '())
(define fenv.global '())

(define-syntax definitial 
  (syntax-rules ()
    ((definitial name)
     (begin (set! env.global (cons (cons 'name 'void) env.global))
            'name ) )
    ((definitial name value)
     (begin (set! env.global (cons (cons 'name value) env.global))
            'name ) ) ) )

(define-syntax definitial-function
  (syntax-rules ()
    ((definitial-function name)
     (begin (set! fenv.global (cons (cons 'name 'void) fenv.global))
            'name ) )
    ((definitial-function name value)
     (begin (set! fenv.global (cons (cons 'name value) fenv.global))
            'name ) ) ) )

(define-syntax defprimitive 
  (syntax-rules ()
   ((defprimitive name value arity)
    (definitial-function name
      (lambda (values)
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

(defprimitive call/cc 
  (lambda (f) 
    (call/cc (lambda (g) 
               (invoke 
                f (list (lambda (values)
                          (if (= (length values) 1)
                              (g (car values))
                              (wrong "Incorrect arity" g) ) ))) )) )
  1 )   

(definitial-function apply 
  (lambda (values)
    (if (>= (length values) 2)
        (let ((f (car values))
              (args (let flat ((args (cdr values)))
                      (if (null? (cdr args))
                          (car args)
                          (cons (car args) (flat (cdr args))) ) )) )
        (invoke f args) )
        (wrong "Incorrect arity" 'apply) ) ) )

(definitial-function list 
  (lambda (values) values) )

(define (chapter2-scheme)
  (define (toplevel)
    (display (f.evaluate (read) env.global fenv.global))
    (toplevel) )
  (toplevel) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;;  Tests

(define (scheme2a)
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
       (print (f.evaluate (skip-read) env.global fenv.global)) ) ) ) )

(define (test-scheme2a file)
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
       (check (f.evaluate (skip-read) env.global fenv.global)) ) )
   (lambda (expected obtained)
     (or (equal? expected obtained)
         (and (eq? obtained the-false-value) (eq? expected #f)) ) ) ) )

(define *tests-to-skip*
  '(
    ) )

(define (test-chap2a file)
  (and (test-scheme2a file)
       (begin (set! evaluate-application evaluate-application2)
              (set! *tests-to-skip*
                    '( ((if #t cons list) 1 22)
                      ) )
              (test-scheme2a file) ) ) )

;;; end of chap2a.scm
