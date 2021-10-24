;;; $Id: chap2c.scm,v 4.2 2006/11/09 18:56:31 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Addition of dynamic variables.
;;; special forms dynamic, dynamic-let, dynamic-set!

(define (df.evaluate e env fenv denv)
  (if (atom? e) 
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e)
                 (boolean? e) (vector? e) )
             e )
            (else (wrong "Cannot evaluate" e)) )
      (case (car e)
        ((quote) (cadr e))
        ((if) (if (df.evaluate (cadr e) env fenv denv)
                  (df.evaluate (caddr e) env fenv denv)
                  (df.evaluate (cadddr e) env fenv denv) ))
        ((begin) (df.eprogn (cdr e) env fenv denv))
        ((set!) (update! (cadr e) 
                         env 
                         (df.evaluate (caddr e) env fenv denv) ))
        ((function)
         (cond ((symbol? (cadr e))
                (f.lookup (cadr e) fenv) )
               ((and (pair? (cadr e)) (eq? (car (cadr e)) 'lambda))
                (df.make-function 
                 (cadr (cadr e)) (cddr (cadr e)) env fenv ) )
               (else (wrong "Incorrect function" (cadr e))) ) )
        ((dynamic) (lookup (cadr e) denv))
        ((dynamic-set!) 
         (update! (cadr e) 
                  denv 
                  (df.evaluate (caddr e) env fenv denv) ) )
        ((dynamic-let)
         (df.eprogn (cddr e)
                    env 
                    fenv
                    (extend denv 
                            (map car (cadr e))
                            (map (lambda (e) 
                                   (df.evaluate e env fenv denv) )
                                 (map cadr (cadr e)) ) ) ) )
        (else (df.evaluate-application 
               (car e) 
               (df.evlis (cdr e) env fenv denv)
               env 
               fenv
               denv )) ) ) )

(define (df.evaluate-application fn args env fenv denv)
  (cond ((symbol? fn) ((f.lookup fn fenv) args denv) )
        ((and (pair? fn) (eq? (car fn) 'lambda))
         (df.eprogn (cddr fn)
                    (extend env (cadr fn) args)
                    fenv
                    denv ) )
        (else (wrong "Incorrect functional term" fn)) ) )

(define (df.make-function variables body env fenv)
  (lambda (values denv)
    (df.eprogn body (extend env variables values) fenv denv) ) )

(define (df.eprogn e* env fenv denv)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (begin (df.evaluate (car e*) env fenv denv)
                 (df.eprogn (cdr e*) env fenv denv) )
          (df.evaluate (car e*) env fenv denv)  )
      empty-begin ) )

(define (df.evlis e* env fenv denv)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (cons (df.evaluate (car e*) env fenv denv)
                (df.evlis (cdr e*) env fenv denv) )
          (list (df.evaluate (car e*) env fenv denv)) )
      '() ) )

;;; Initial environment

(define env.global '())
(define fenv.global '())
(define denv.global '())

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

(defprimitive call/cc 
  (lambda (f) 
    (call/cc (lambda (g) 
               (f (lambda (values denv)
                    (if (= (length values) 1)
                        (g (car values))
                        (wrong "Incorrect arity" g) ) )
                  denv ) )) )
  1 )   

(definitial-function apply 
  (lambda (values denv)
    (if (>= (length values) 2)
        (let ((f (car values))
              (args (let flat ((args (cdr values)))
                      (if (null? (cdr args))
                          (car args)
                          (cons (car args) (flat (cdr args))) ) )) )
        (f args denv) )
        (wrong "Incorrect arity" 'apply) ) ) )

(definitial-function list 
  (lambda (values denv) values) )


;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;;  Tests

(define (scheme2c)
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
       (print (df.evaluate (skip-read) env.global fenv.global denv.global)) ) ) ) )

(define (test-scheme2c file)
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
       (check (df.evaluate (skip-read) env.global fenv.global denv.global)) ) )
   (lambda (expected obtained)
     (or (equal? expected obtained)
         (and (eq? obtained the-false-value) (eq? expected #f)) ) ) ) )

(define *tests-to-skip*
  '(
    ) )

;;; end of chap2c.scm
