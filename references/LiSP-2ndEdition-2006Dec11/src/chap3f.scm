;;; $Id: chap3f.scm,v 4.0 1995/07/10 06:51:14 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; An interpreter with objects from chapter 3.

(define (evaluate e r k)
  (if (atom? e)
    (cond ((symbol? e) (evaluate-variable e r k))
          (else        (evaluate-quote e r k)) )
    (case (car e)
     ((quote)  (evaluate-quote (cadr e) r k))
     ((if)     (evaluate-if (cadr e) (caddr e) (cadddr e) r k))
     ((begin)  (evaluate-begin (cdr e) r k))
     ((set!)   (evaluate-set! (cadr e) (caddr e) r k))
     ((lambda) (evaluate-lambda (cadr e) (cddr e) r k))
     (else     (evaluate-application (car e) (cdr e) r k)) ) ) )

(define-class value Object ())

(define-class environment Object ())

(define-class continuation Object (k))

(define-class null-env environment ())

(define-class full-env environment (others name))

(define-class variable-env full-env (value)) 

(define-generic (invoke (f) v* r k)
  (wrong "not a function" f r k) )

(define-generic (resume (k continuation) v)
  (wrong "Unknown continuation" k) )

(define-generic (lookup (r environment) n k)
  (wrong "not an environment" r n k) )

(define-generic (update! (r environment) n k v)
  (wrong "not an environment" r n k) )


(define (evaluate-quote v r k)
  (resume k v) )

(define-class if-cont continuation (et ef r))

(define (evaluate-if ec et ef r k)
  (evaluate ec r (make-if-cont k 
                               et 
                               ef 
                               r )) )

(define-method (resume (k if-cont) v)
  (evaluate (if v (if-cont-et k) (if-cont-ef k)) 
            (if-cont-r k)
            (if-cont-k k) ) )

(define-class begin-cont continuation (e* r))

(define (evaluate-begin e* r k)
  (if (pair? e*)
    (if (pair? (cdr e*))
      (evaluate (car e*) r (make-begin-cont k e* r))
      (evaluate (car e*) r k) )
    (resume k empty-begin-value) ) )

(define-method (resume (k begin-cont) v)
  (evaluate-begin (cdr (begin-cont-e* k)) 
                  (begin-cont-r k) 
                  (begin-cont-k k) ) )

(define (evaluate-variable n r k)
  (lookup r n k) ) 

(define-method (lookup (r null-env) n k)
  (wrong "Unknown variable" n r k) )

(define-method (lookup (r full-env) n k)
  (lookup (full-env-others r) n k) )

(define-method (lookup (r variable-env) n k)
  (if (eqv? n (variable-env-name r))
    (resume k (variable-env-value r))
    (lookup (variable-env-others r) n k) ) )

(define-class set!-cont continuation (n r))

(define (evaluate-set! n e r k)
  (evaluate e r (make-set!-cont k n r)) )

(define-method (resume (k set!-cont) v)
  (update! (set!-cont-r k) (set!-cont-n k) (set!-cont-k k) v) )

(define-method (update! (r null-env) n k v)
  (wrong "Unknown variable" n r k) )

(define-method (update! (r full-env) n k v)
  (update! (full-env-others r) n k v) )

(define-method (update! (r variable-env) n k v)
  (if (eqv? n (variable-env-name r))
    (begin (set-variable-env-value! r v)
           (resume k v) )
    (update! (variable-env-others r) n k v) ) )

(define-class function value (variables body env))

(define (evaluate-lambda n* e* r k)
  (resume k (make-function n* e* r)) )

(define-method (invoke (f function) v* r k)
  (let ((env (extend-env (function-env f)
                         (function-variables f)
                         v* )))
    (evaluate-begin (function-body f) env k) ) )

(define (extend-env env names values)
  (cond ((and (pair? names) (pair? values))      
         (make-variable-env  
          (extend-env env (cdr names) (cdr values))
          (car names)
          (car values) ) )
        ((and (null? names) (null? values)) env)
        ((symbol? names) (make-variable-env env names values))
        (else (wrong "Arity mismatch")) ) )

(define-class evfun-cont continuation (e* r))

(define-class apply-cont continuation (f r))

(define-class argument-cont continuation (e* r))

(define-class gather-cont continuation (v))

(define (evaluate-application e e* r k)
  (evaluate e r (make-evfun-cont k e* r)) )

(define-method (resume (k evfun-cont) f)
  (evaluate-arguments (evfun-cont-e* k)
                      (evfun-cont-r k)
                      (make-apply-cont (evfun-cont-k k)
                                       f
                                       (evfun-cont-r k) ) ) )

(define (evaluate-arguments e* r k)
  (if (pair? e*)
    (evaluate (car e*) r (make-argument-cont k e* r))
    (resume k no-more-arguments) ) )

(define no-more-arguments '())

(define-method (resume (k argument-cont) v)
  (evaluate-arguments (cdr (argument-cont-e* k)) 
                      (argument-cont-r k)
                      (make-gather-cont (argument-cont-k k) v)) )

(define-method (resume (k gather-cont) v*)
  (resume (gather-cont-k k) (cons (gather-cont-v k) v*)) )

(define-method (resume (k apply-cont) v)
  (invoke (apply-cont-f k) 
          v
          (apply-cont-r k)
          (apply-cont-k k) ) )

(define-class bottom-cont continuation (f))

(define-method (resume (k bottom-cont) v)
  ((bottom-cont-f k) v) )

(define-class primitive value (name address))

(define-method (invoke (f primitive) v* r k)
  ((primitive-address f) v* r k) )

(define-method (invoke (f continuation) v* r k)
  (if (= 1 (length v*))
      (resume f (car v*))
      (wrong "Continuations expect one argument" v* r k) ) )

(define (chapter3-interpreter)
  (define (toplevel)
    (evaluate (read) 
              r.init 
              (make-bottom-cont 'void display) )
    (toplevel) )
  (toplevel) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Initial environment

(define r.init (make-null-env))

(define-syntax definitial 
  (syntax-rules ()
    ((definitial name)
     (definitial name 'void) )
    ((definitial name value)
     (begin (set! r.init (make-variable-env r.init 'name value))
            'name ) ) ) )

(define-syntax defprimitive 
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial name 
       (make-primitive 
        'name (lambda (v* r k) 
                (if (= arity (length v*))
                    (resume k (apply value v*))
                    (wrong "Incorrect arity" 'name v*) ) ) ) ) ) ) )

(define-syntax defpredicate 
  (syntax-rules ()
    ((defpredicate name value arity)
     (defprimitive name
       (lambda values (apply value values))
       arity ) ) ) )

(definitial t #t)
(definitial f #f)
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

(definitial call/cc 
  (make-primitive 
   'call/cc
   (lambda (v* r k) 
     (if (= 1 (length v*))
         (invoke (car v*) (list k) r k)
         (wrong "Incorrect arity" 'call/cc v*) ) ) ) )

(definitial apply
  (make-primitive 
   'apply
   (lambda (v* r k)
     (if (>= (length v*) 2)
         (let ((f (car v*))
               (args (let flat ((args (cdr v*)))
                       (if (null? (cdr args))
                           (car args)
                           (cons (car args) (flat (cdr args))) ) )) )
           (invoke f args r k) )
         (wrong "Incorrect arity" 'apply) ) ) ) )
             
(definitial list 
  (make-primitive
   'list
   (lambda (v* r k) (resume k v*)) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;;  Tests

(define (scheme3f)
  (interpreter
   "Scheme? "
   "Scheme= "
   #t
   (lambda (read print error)
     (define k.init (make-bottom-cont 'void print))
     (set! wrong error)
     (lambda ()
       (print (evaluate (read) r.init k.init)) ) ) ) )

(define (test-scheme3f file)
  (suite-test
   file
   "Scheme? "
   "Scheme= "
   #t
   (lambda (read check error)
     (define k.init (make-bottom-cont 'void check))
     (set! wrong error)
     (lambda ()
       (check (evaluate (read) r.init k.init)) ) )
   equal? ) )

;;; end of chap3f.scm
