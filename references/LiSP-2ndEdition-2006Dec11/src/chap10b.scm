;;; $Id: chap10b.scm,v 4.0 1995/07/10 06:50:34 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; This file defines a quick and dirty interpreter to test chap10a.scm
;;; It directly interprets objectified code.

(define-generic (evaluate (e Program) sr))

(define-method (evaluate (e Local-Reference) sr)
  (cdr (assq (Local-Reference-variable e) sr)) )

(define undefined-value (cons 'undefined 'value))

(define-method (evaluate (e Global-Reference) sr)
  (let ((v (cdr (assq (Global-Reference-variable e) sg.global))))
    (if (eq? v undefined-value)
        (evaluate-error "Unbound variable" e)
        v ) ) )

(define-method (evaluate (e Global-Assignment) sr)
  (let ((v (assq (Global-Assignment-variable e) sg.global))
        (value (evaluate (Global-Assignment-form e) sr)) )
    (set-cdr! v value) ) )

(define-method (evaluate (e Local-Assignment) sr)
  (let* ((ref   (Local-Assignment-reference e))
         (v     (assq (Reference-variable ref) sr))
         (value (evaluate (Local-Assignment-form e) sr)) )
    (set-cdr! v value) ) )

(define-method (evaluate (e Predefined-Reference) sr)
  (cdr (assq (Predefined-Reference-variable e) sg.init)) )

;;; Boxes are coded in the cdr of pairs, why not !

(define-method (evaluate (e Box-Read) sr)
  (cdr (evaluate (Box-read-reference e) sr)) )

(define-method (evaluate (e Box-Write) sr)
  (set-cdr! (evaluate (Box-Write-reference e) sr)
            (evaluate (Box-Write-form e) sr) ) )

(define-method (evaluate (e Box-Creation) sr)
  (let ((v (assq (Box-Creation-variable e) sr)))
    (set-cdr! v (cons 'box (cdr v))) ) )

(define-class RunTime-Procedure Object (body variables environment))
(define-class RunTime-Primitive Object (address comparator arity))

(define-method (evaluate (e Function) sr)
  (make-RunTime-Procedure (Function-body e)
                          (Function-variables e)
                          sr ) )

(define-method (evaluate (e Alternative) sr)
  (if (evaluate (Alternative-condition e) sr)
      (evaluate (Alternative-consequent e) sr)
      (evaluate (Alternative-alternant e) sr) ) )

(define-method (evaluate (e Sequence) sr)
  (begin (evaluate (Sequence-first e) sr)
         (evaluate (Sequence-last e) sr) ) )

(define-method (evaluate (e Constant) sr)
  (Constant-value e) )

(define-method (evaluate (e Arguments) sr)
  (cons (evaluate (Arguments-first e) sr)
        (evaluate (Arguments-others e) sr) ) )

(define-method (evaluate (e No-Argument) sr)
  '() )

(define-method (evaluate (e Regular-Application) sr)
  (let ((f (evaluate (Regular-Application-function e) sr))
        (args (evaluate (Regular-Application-arguments e) sr)) )
    (invoke f args) ) )

(define-generic (invoke (f) args))

(define-method (invoke (f RunTime-Procedure) args)
  (unless (let check ((variables (RunTime-Procedure-variables f))
                      (args args) )
            (if (pair? variables)
                (or (Local-Variable-dotted? (car variables))
                    ;; (car variables) is a regular variable
                    (and (pair? args)
                         (check (cdr variables) (cdr args)) ) )
                (not (pair? args)) ) )
    (evaluate-error "Wrong arity" f args) )
  (evaluate (RunTime-Procedure-body f)
            (sr-extend (RunTime-Procedure-environment f)
                       (RunTime-Procedure-variables f) 
                       args ) ) )

(define-method (invoke (f RunTime-Primitive) args)
  (unless ((RunTime-Primitive-comparator f) 
           (length args) (RunTime-Primitive-arity f) )
    (evaluate-error "Wrong arity" f args) )
  (apply (RunTime-Primitive-address f) args) )

(define (sr-extend sr variables values)
  (if (pair? variables)
      (if (Local-Variable-dotted? (car variables))
          ;; don't recurse, it's a Dotted-Variable (and necesserarily
          ;; the last one).
          (cons (cons (car variables) values)
                sr )
          ;; a regular variable
          (if (pair? values)
              (cons (cons (car variables) (car values))
                    (sr-extend sr (cdr variables) (cdr values)) )
              (evaluate-error "Not enough values" variables) ) )
      (if (null? values) 
          sr
          (evaluate-error "Too much values" values) ) ) )

(define-method (evaluate (e Predefined-Application) sr)
  (let ((f (cdr (assq (Predefined-Application-variable e) sg.init)))
        (args (evaluate (Predefined-Application-arguments e) sr)) )
    (invoke f args) ) )

(define-method (evaluate (e Fix-Let) sr)
  (let ((args (evaluate (Fix-Let-arguments e) sr)))
    (evaluate (Fix-Let-body e) 
              (sr-regular-extend sr (Fix-Let-variables e) args) ) ) )

;;; (assume (every? Regular-Variable? variables))

(define (sr-regular-extend sr variables values)
  (if (pair? variables)
      (if (pair? values)
          (cons (cons (car variables) (car values))
                (sr-regular-extend sr (cdr variables) (cdr values)) )
          (evaluate-error "Not enough values" variables) )
      (if (null? values) sr
          (evaluate-error "Too much values" values) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Initial runtime environment

(define sg.init '())

(define sg.global '())

(define sg.current '())

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Initial compile-time environment

;;; The global user mutable environment:

(define g.current '())

;;; the predefined immutable global environment:

(define g.init '())

(define-syntax definitial
  (syntax-rules ()
    ((definitial name value)
     (let ((v (make-Predefined-Variable 'name #f)))
       (set! g.init (cons v g.init))
       (set! sg.init (cons (cons v value) sg.init))
       'name ) ) ) )

(define-syntax defprimitive
  (syntax-rules (>=0 >=2)
    ((defprimitive name value 0)
     (let ((v (make-Predefined-Variable 
               'name (make-Functional-Description = 0 "") ))
           (f (make-RunTime-Primitive value = 0)) )
       (set! g.init (cons v g.init))
       (set! sg.init (cons (cons v f) sg.init))
       'name ) )
    ((defprimitive name value 1)
     (let ((v (make-Predefined-Variable 
               'name (make-Functional-Description = 1 "") ))
           (f (make-RunTime-Primitive value = 1)) )
       (set! g.init (cons v g.init))
       (set! sg.init (cons (cons v f) sg.init))
       'name ) )
    ((defprimitive name value 2)
     (let ((v (make-Predefined-Variable 
               'name (make-Functional-Description = 2 "") ))
           (f (make-RunTime-Primitive value = 2)) )
       (set! g.init (cons v g.init))
       (set! sg.init (cons (cons v f) sg.init))
       'name ) )
    ((defprimitive name value 3)
     (let ((v (make-Predefined-Variable 
               'name (make-Functional-Description = 3 "") ))
           (f (make-RunTime-Primitive value = 3)) )
       (set! g.init (cons v g.init))
       (set! sg.init (cons (cons v f) sg.init))
       'name ) )
    ((defprimitive name value >=0)
     (let ((v (make-Predefined-Variable 
               'name (make-Functional-Description >= 0 "") ))
           (f (make-RunTime-Primitive value >= 0)) )
       (set! g.init (cons v g.init))
       (set! sg.init (cons (cons v f) sg.init))
       'name ) )
    ((defprimitive name value >=2)
     (let ((v (make-Predefined-Variable 
               'name (make-Functional-Description >= 2 "") ))
           (f (make-RunTime-Primitive value >= 2)) )
       (set! g.init (cons v g.init))
       (set! sg.init (cons (cons v f) sg.init))
       'name ) ) ) )
  
;;; Define a location in the user global environment.

(define-syntax defvariable
  (syntax-rules ()
    ((defvariable name)
     (let ((v (make-Global-Variable 'name)))
      (set! g.current (cons v g.current))
      (set! sg.current (cons (cons v undefined-value) sg.current))
       'name ) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Initialization of the predefined global environment.

(definitial t #t)
(definitial f #f)
(definitial nil '())

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defprimitive pair? pair? 1)
(defprimitive symbol? symbol? 1)
(defprimitive eq? eq? 2)
(defprimitive set-car! set-car! 2)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defprimitive = = 2)
(defprimitive < < 2)
(defprimitive > > 2)
(defprimitive * * 2)
(defprimitive <= <= 2)
(defprimitive >= >= 2)
(defprimitive remainder remainder 2)
(defprimitive display display 1)

(defprimitive apply
  (lambda (f . args)
    (if (pair? args)
        (invoke f (let flat ((args args))
                    (if (null? (cdr args)) (car args)
                        (cons (car args) (flat (cdr args))) ) ))
        (evaluate-error "incorrect arity" 'apply) ) )
  >=2 )

(defprimitive call/cc
  (lambda (f)
    (call/cc (lambda (k) (invoke f (list (make-RunTime-Primitive
                                          k = 1 ))))) )
  1 )

(defprimitive list
  (lambda args args)
  >=0 )    

(defvariable a)
(defvariable foo)
(defvariable fact)
(defvariable primes)

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Test suite

(define (compile-expression e)
  (Sexp->object e) )

(define (scheme10b)
  (set! sg.global sg.current)
  (interpreter
   "Scheme? "
   "Scheme= "
   #t
   (lambda (read print error)
     (set! objectify-error error)
     (set! evaluate-error error)
     (lambda ()
       (print (evaluate (compile-expression (read)) '())) ) ) ) )

(define (test-scheme10b file)
  (set! sg.global sg.current)
  (suite-test
   file
   "Scheme? "
   "Scheme= "
   #t
   (lambda (read check error)
     (set! objectify-error error)
     (set! evaluate-error error)
     (lambda ()
       (check (evaluate (compile-expression (read)) '())) ) )
   naive-match ) )

;;; end of chap10b.scm
