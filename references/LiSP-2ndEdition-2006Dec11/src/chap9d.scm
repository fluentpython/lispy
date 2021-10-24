;;; $Id: chap9d.scm,v 4.0 1995/07/10 06:52:20 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; This file defines a quick and dirty interpreter to test chap9c.scm
;;; It directly interprets objectified code. It is based on
;;; chap10b.scm file. Here the environment SR contains everything from
;;; local variables to global mutable or predefined bindings.

(define-generic (evaluate (e Program) sr))

(define-method (evaluate (e Local-Reference) sr)
  (let ((v (assq (Local-Reference-variable e) sr)))
    (if (pair? v) (cdr v)
        (evaluate-error "Unexistant variable" e) ) ) )

(define undefined-value (cons 'undefined 'value))

(define-method (evaluate (e Global-Reference) sr)
  (let ((v (assq (Global-Reference-variable e) sr)))
    (if (pair? v)
        (let ((value (cdr v)))
          (if (eq? value undefined-value)
              (evaluate-error "Uninitialized variable" e)
              value ) )
        (evaluate-error "Unexistant variable" e) ) ) )

(define-method (evaluate (e Global-Assignment) sr)
  (let ((v (assq (Global-Assignment-variable e) sr)))
    (if (pair? v)
        (let ((value (evaluate (Global-Assignment-form e) sr)))
          (set-cdr! v value) )
        (evaluate-error "Unexistant variable" e) ) ) )

(define-method (evaluate (e Local-Assignment) sr)
  (let* ((ref   (Local-Assignment-reference e))
         (v     (assq (Reference-variable ref) sr))
         (value (evaluate (Local-Assignment-form e) sr)) )
    (if (pair? v)
        (set-cdr! v value)
        (evaluate-error "Unexistant variable" e) ) ) )

(define-method (evaluate (e Predefined-Reference) sr)
  (cdr (assq (Predefined-Reference-variable e) sr)) )


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
            (sr-extend* (RunTime-Procedure-environment f)
                        (RunTime-Procedure-variables f) 
                        args ) ) )

(define-method (invoke (f RunTime-Primitive) args)
  (unless ((RunTime-Primitive-comparator f) 
           (length args) (RunTime-Primitive-arity f) )
    (evaluate-error "Wrong arity" f args) )
  (apply (RunTime-Primitive-address f) args) )

(define (sr-extend* sr variables values)
  (if (pair? variables)
      (if (Local-Variable-dotted? (car variables))
          ;; don't recurse, it's a Dotted-Variable (and necesserarily
          ;; the last one).
          (sr-extend sr (car variables) values)
          ;; a regular variable
          (if (pair? values)
              (sr-extend (sr-extend* sr (cdr variables) (cdr values))
                         (car variables) (car values) )
              (evaluate-error "Not enough values" variables) ) )
      (if (null? values) 
          sr
          (evaluate-error "Too much values" values) ) ) )

(define (sr-extend sr variable value)
  (cons (cons variable value) sr) )

(define-method (evaluate (e Predefined-Application) sr)
  (let ((f (cdr (assq (Predefined-Application-variable e) sr)))
        (args (evaluate (Predefined-Application-arguments e) sr)) )
    (invoke f args) ) )

(define-method (evaluate (e Fix-Let) sr)
  (let ((args (evaluate (Fix-Let-arguments e) sr)))
    (evaluate (Fix-Let-body e) 
              (sr-regular-extend* sr (Fix-Let-variables e) args) ) ) )

;;; (assume (every? Regular-Variable? variables))

(define (sr-regular-extend* sr variables values)
  (if (pair? variables)
      (if (pair? values)
          (sr-extend (sr-regular-extend* sr (cdr variables) (cdr values))
                     (car variables) (car values) )
          (evaluate-error "Not enough values" variables) )
      (if (null? values) sr
          (evaluate-error "Too much values" values) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Initial runtime environment containing only predefined bindings.

(define g.predef  '())
(define sg.predef '())

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Initial compile-time environment

(define-syntax definitial
  (syntax-rules ()
    ((definitial name value)
     (let ((v (make-Predefined-Variable 'name #f)))
       (set! g.predef  (make-Full-Environment g.predef v))
       (set! sg.predef (sr-extend sg.predef v value))
       'name ) ) ) )

(define-syntax defprimitive
  (syntax-rules (>=0 >=2)
    ((defprimitive name value 0)
     (let ((v (make-Predefined-Variable 
               'name (make-Functional-Description = 0 "") ))
           (f (make-RunTime-Primitive value = 0)) )
       (set! g.predef  (make-Full-Environment g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
       'name ) )
    ((defprimitive name value 1)
     (let ((v (make-Predefined-Variable 
               'name (make-Functional-Description = 1 "") ))
           (f (make-RunTime-Primitive value = 1)) )
       (set! g.predef  (make-Full-Environment g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
       'name ) )
    ((defprimitive name value 2)
     (let ((v (make-Predefined-Variable 
               'name (make-Functional-Description = 2 "") ))
           (f (make-RunTime-Primitive value = 2)) )
       (set! g.predef  (make-Full-Environment g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
       'name ) )
    ((defprimitive name value 3)
     (let ((v (make-Predefined-Variable 
               'name (make-Functional-Description = 3 "") ))
           (f (make-RunTime-Primitive value = 3)) )
       (set! g.predef  (make-Full-Environment g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
       'name ) )
    ((defprimitive name value >=0)
     (let ((v (make-Predefined-Variable 
               'name (make-Functional-Description >= 0 "") ))
           (f (make-RunTime-Primitive value >= 0)) )
       (set! g.predef  (make-Full-Environment g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
       'name ) )
    ((defprimitive name value >=2)
     (let ((v (make-Predefined-Variable 
               'name (make-Functional-Description >= 2 "") ))
           (f (make-RunTime-Primitive value >= 2)) )
       (set! g.predef  (make-Full-Environment g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
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
(defprimitive atom? atom? 1)
(defprimitive symbol? symbol? 1)
(defprimitive null? null? 1)
(defprimitive not not 1)
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
(defprimitive modulo modulo 2)
(defprimitive display show 1)
(defprimitive newline newline 0)

;;; Additional things, append is needed by quasiquote
(defprimitive append append 2)
;;; and gensym by some tests.
(defprimitive gensym gensym 0)
(defprimitive list-ref list-ref 2)
(defprimitive list-tail list-tail 2)

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

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define global-runtime-environment-mark (list (gensym)))

(define (find-global-runtime-environment sg)
  (if (eq? (car sg) global-runtime-environment-mark)
      sg
      (find-global-runtime-environment (cdr sg)) ) )

(define (mark-global-runtime-environment sg)
  (cons global-runtime-environment-mark sg) )

;;; Enrich the runtime environment with the new global locations that
;;; appeared in g.

(define (enrich-with-new-global-variables! level)
  (let* ((g (find-global-environment 
             (Evaluator-Preparation-Environment level) ) )
         (sg-head (find-global-runtime-environment
                   (Evaluator-RunTime-Environment level) )) )
    (let loop ((g (Environment-next g)))
      (when (Full-Environment? g)
        (let ((var (Full-Environment-variable g)))
          (when (and (Global-Variable? var) (not (pair? (assq var sg-head))))
            (set-cdr! sg-head (sr-extend (cdr sg-head) var undefined-value)) ) )
        (loop (Full-Environment-next g))) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; debug utilities

(define-method (show (o Full-Environment) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (format stream "#<Full-Environment ")
    (show (Full-Environment-variable o) stream)
    (format stream ", ")
    (show (Full-Environment-next o) stream)
    (format stream ">") ) )

(define-method (show (o Variable) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (format stream "#<~A ~A>" 
            (Class-name (object->class o))
            (Variable-name o) ) ) )

(define-method (show (o Magic-Keyword) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (format stream "#<Magic:~A>" (Magic-Keyword-name o)) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Test suite

(define (scheme9d)
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
     (set! objectify-error error)
     (set! evaluate-error error)
     (let ((eval (Evaluator-eval (create-evaluator #f))))
       (lambda ()
         (print (eval (skip-read))) ) ) ) ) )

(define (test-scheme9d file)
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
     (set! objectify-error error)
     (set! evaluate-error error)
     (let ((eval (Evaluator-eval (create-evaluator #f))))
       (lambda ()
         (check (eval (skip-read))) ) ) )
   naive-match ) )

(define *tests-to-skip*
  '( xyzzy
     (set! xyzzy 3)
     ((lambda (x y) xyzzy) 1 2)
    ) )

;;; end of chap9d.scm
