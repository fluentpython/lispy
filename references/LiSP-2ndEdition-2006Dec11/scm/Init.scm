;;; $Id: Init.scm,v 1.8 1994/09/02 15:02:35 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; This file is used to customize scm (Aubrey Jaffer's Scheme
;;; interpreter) in order to run the source files of the book. It
;;; preloads Meroonet, and the test-driver. It may serve as a basis
;;; for other ports to other Scheme interpreters. Look for the
;;; IMPORTANT notes below.

;;; This variable is used in chap8k.scm to determine the underlying
;;; Scheme interpreter.

(define book-interpreter-support 'scm)

;;; **IMPORTANT 1(but easy)**
;;; Missing functions:

(define call/cc call-with-current-continuation)
(define (gensym . ignore) (gentemp))
(define (atom? x) (not (pair? x)))

;;; I also like the iota function. (iota 0 4) -> (0 1 2 3)

(define (iota start end)
  (if (< start end)
      (cons start (iota (+ 1 start) end))
      '() ) )

(define (last-pair l)
  (if (pair? l)
      (if (pair? (cdr l)) 
          (last-pair (cdr l))
          l )
      (support-error 'last-pair l) ) )

(define (every? p . args)
  (let andmap ((args args) (value #t))
    (if (let any-at-end? ((ls args))
          (and (pair? ls)
               (or (not (pair? (car ls)))
                   (any-at-end? (cdr ls)))))
        value
        (let ((value (apply p (map car args))))
          (and value (andmap (map cdr args) value))))))

(define (any? p . args)
  (let ormap ((args args) (value #f))
    (if (let any-at-end? ((ls args))
          (and (pair? ls)
               (or (not (pair? (car ls)))
                   (any-at-end? (cdr ls)))))
        value
        (let ((value (apply p (map car args))))
          (or value (ormap (map cdr args) value))))))

(define (reverse! l)
  (define (nreverse l r)
    (if (pair? l)
       (let ((cdrl (cdr l)))
         (set-cdr! l r)
         (nreverse cdrl l) )
       r ) )
  (nreverse l '()) )

(define (symbol-append . args)
  (string->symbol
   (apply string-append
          (map (lambda (s)
                 (cond ((string? s) s)
                       ((symbol? s) (symbol->string s))
                       ((number? s) (number->string s))
                       (else (error 'symbol-append args)) ) )
               args ) ) ) )

;;; Name the Un*x ports and flush them.

(define stdout-port (current-output-port))
(define stderr-port (current-error-port))

(define flush-buffer force-output)

;;; Quick and dirty: sometimes very big objects are printed, limit
;;; them to something affordable.

(define *bounded-length* 4)
(define *bounded-depth* 3)

(define (bounded-display o stream)
  (define (print-list o* len dep)
    (cond ((null? o*) #t)
          ((atom? o*) (display " . " stream)
                      (display o* stream) )
          ((pair? o*) 
           (print (car o*) len (+ dep 1))
           (if (pair? (cdr o*)) (display " " stream))
           (print-list (cdr o*) (+ len 1) dep) ) ) )
  (define (print o len dep)
    (cond ((object? o) 
           (display "#<" stream)
           (display (Class-name (object->class o)) stream)
           (display ">" stream) )
          ((atom? o) (display o stream))
           (else (if (or (> len *bounded-length*)
                         (> dep *bounded-depth*) )
                     (display "&&&" stream)
                     (begin
                       (display "(" stream)
                       (print-list o len dep)
                       (display ")" stream) ) ) ) ) )
  (print o 0 0) )
;;; Test: (bounded-display (call-with-input-file "si/reflisp.scm" read) stdout-port)

;;; Sometimes property lists are seldom used.
;;; Use the implementation of Ozan Yigit (see Scheme Repository)

(load "scm/others/prop.scm")
(define putprop put)
(define getprop get)

;;; require additional utilities
(require 'format)
(require 'pretty-print)
(define pp pp:pretty-print)

;;; Loads syntax-case V2.0.  It could have been possible, in SCM, to
;;; (require 'syntax-case) but I choose to keep V2.0 and to show
;;; explicitely those files that have to be loaded. The goal is to
;;; provide a new expansion algorithm provided by the expand-syntax
;;; function but pay attention, this is not the current macroexpander.

(load "scm/others/compat.ss")
(load "scm/others/hooks.scm")
(load "scm/others/output.ss")
(load "scm/others/init.ss")
(load "scm/others/expand.bb")
(load "scm/others/macro-defs.bb")

;;; Two macros that I frequently use:
;;; Since, the macroexpander of the current load in progress is still not
;;; aware of the change of macroexpansion algorithm, invoke expand-syntax
;;; directly to register them.

(expand-syntax
 '(begin
    (define-syntax unless
      (syntax-rules ()
        ((unless condition form ...)
         (if (not condition) (begin form ...)) ) ) )
    (define-syntax when
      (syntax-rules ()
        ((when condition form ...)
         (if condition (begin form ...)) ) ) ) ) )

;;; This function loads a file expanded with syntax-expand.

(define *syntax-case-load-verbose?* #f)

(define (syntax-case-load file)
  (call-with-input-file file
    (lambda (in)
      (if *syntax-case-load-verbose?* 
          (begin (newline)
                 (display ";;; Loading ")
                 (display file)
                 (newline) ) )
      (let loop ((e (read in)))
        (if (eof-object? e) 
            file
            (let ((r (eval (expand-syntax e))))
              (if *syntax-case-load-verbose?*
                  (begin (display ";= ")
                         (display r)
                         (newline) ) )
              (loop (read in)) ) ) ) ) ) )

;;; Defines specific locations for error handlers for meroonet.scm and
;;; tester.scm. They will be filled later.

(define meroonet-error 'wait)

(define tester-error 'wait)

;;; Load the test-driver (fortunately it does not need macros, so use
;;; load instead of syntax-case-load).

(load "src/tester.scm")

;;; Load Meroonet. Meroonet defines three macros with
;;; define-meroonet-macro. These macros are not needed by the rest of
;;; this file, so it is sufficient to register them with syntax-case.

(expand-syntax
 '(define-syntax define-meroonet-macro
    (lambda (y)
      ;; snarfed from syntax-caseV2.0/structure.ss
      (define construct-name
         (lambda (template-identifier . args)
            (implicit-identifier
               template-identifier
               (string->symbol
                  (apply string-append
                         (map (lambda (x)
                                 (if (string? x)
                                     x
                                     (symbol->string (syntax-object->datum x))))
                              args))))))
      (syntax-case y ()
        ((_ (name . variables) . body)
         (with-syntax 
           ((expander (construct-name (syntax name) (syntax name) "-expander")))
           (syntax
            (begin (define (expander x)
                     ;; x receives a syntax-object and not a form
                     (define (strip x) (vector-ref x 1))
                     ;; Don't check arity, apply directly
                     (apply (lambda variables . body)
                            (cdr (strip x)) ) )
                   ;; Oddly enough, writing expander alone hurts !?
                   ;; so delay it in a lambda.
                   (define-syntax name 
                     (lambda a (apply expander a)) ) ) ) ) ) ) ) ) )

(syntax-case-load "meroonet/meroonet.scm")

;;; **IMPORTANT 2**
;;; The test-driver should try to catch errors of the underlying Scheme 
;;; system. This is non-portable and difficult in many implementations. If
;;; do not succeed writing it, you can still run the programs of the book
;;; but you will not be able to run all the test-suites since some tests 
;;; (for instance in meroonet/oo-tests.scm) require errors to be caught
;;; when signalled by list-tail with a non-numeric second argument.

;;; This is what I did for SCM.
;;; The old catch-error like feature of MacLisp. Ideally it returns a
;;; list of a single element if body evaluates without error or it
;;; returns a atom in case of error (often a string naming the error).
;;; It should not forbid escapes out of protected-eval (for tester.scm)
;;; that is why there is the *error-already-handled?* flag. An error such
;;; as (list-tail '(a b) 'erroneous) does not call error, nor slib:error
;;; but performs an abort control effect that is caught by the postlude
;;; of the dynamic-wind which goes back to the normal work. But since at 
;;; that time, ,the postlude is always invoked, you must differentiate
;;; the errors explicitely caught by Meroonet from those caught by SCM,
;;; this is what does the *error-already-handled?* flag.

(expand-syntax
 '(define-syntax catch-error
    (syntax-rules ()
      ((catch-error forms ...)
       (protected-eval (lambda () forms ...)) ) ) ) )

(define *error-already-handled?* #f)

(define (protected-eval thunk)
  (let ((result #f)
        (restart #f) )
    (call-with-current-continuation
     (lambda (exit)
       (dynamic-wind
        (lambda () 'nothing)
        (lambda ()
          (call-with-current-continuation
           (lambda (k) (set! restart k)) )
          (if result 
              (begin ;;(write `(caught!))(newline) ; DEBUG
                     (exit result) )
              (begin (set! result (list (thunk)))
                     result ) ) )
        (lambda () 
          (if (or *error-already-handled?* result)
              ;; this is a normal return
              'nothing
              ;; this should catch all underlying errors
              (begin (set! result "Internal error caught")
                     (restart result) ) ) ) ) ) ) ) )

;Test:
;(write (catch-error (cons 1 2))) (newline)
;(write (catch-error (car #t))) (newline)
;(write (catch-error (cons 1 (catch-error (car #t))))) (newline)

;;; **IMPORTANT 3**
;;; The problem now is that catch-error is a syntax-case macro but not
;;; a regular macro for the current macroexpander. So define a 
;;; macro-definer for it.

(defmacro define-abbreviation (call . body)
  `(defmacro ,(car call) ,(cdr call) . ,body) )

;;; define catch-error for the current macroexpander. catch-error will
;;; be used twice in test-meroonet and start below.

(define-abbreviation (catch-error . body)
  `(protected-eval (lambda () . ,body)) )

;;; Since the define-abbreviation is also necessary for the book when
;;; non high level macros are defined, register define-abbreviation
;;; for syntax-case. 

(expand-syntax
 '(define-syntax define-abbreviation
    (syntax-rules ()
      ((define-abbreviation call . body)
       (define-meroonet-macro call . body) ) ) ) )

;;; This function will test a suite of tests. It also allows to test the
;;; port of syntax-case.

(define (test file)
  (suite-test
   file "?? " "== " #t
   (lambda (read check err)
     (define (the-error . args)
       (set! *error-already-handled?* #t)
       (apply err args) )
     (set! *error-already-handled?* #f)
     (set! meroonet-error the-error)
     (set! tester-error    the-error)
     (lambda ()
       (set! *error-already-handled?* #f)
       ;; (read) must not be done under control of catch-error.
       (let ((e (read)))
         (let ((r (catch-error (eval (expand-syntax e)))))
           (if (pair? r) (check (car r)) (err 'test-meroonet r)) ) ) ) )
   equal? ) )
;;; Test: 
;;;	(test "meroonet/oo-tests.scm")
;;;	(test "bigloo/others/syntax.tst")

;;; This variable is needed by meroonet/oo-tests.scm test suite.

(define the-Point 'useful4tests)

;;; Generally, when an error is detected in one of my programs, a
;;; <something>-error function is called which calls itself wrong. The
;;; wrong function allows the test-driver to be aware that something
;;; went wrong.

(define wrong 'wait)
(define static-wrong 'wait)

;;; The `show' and `clone' generic functions are predefined in Meroon
;;; not in Meroonet.  The problem is to define a generic function with
;;; Meroonet macros while these macros are only compiled and not yet
;;; present.

;;; The clone function that performs a shallow copy of a Meroonet object.

(eval (expand-syntax 
       '(begin
          (define-generic (show (o) . stream)
            (let ((stream (if (pair? stream) (car stream)
                              (current-output-port) )))
              (bounded-display o stream) ) )
          (define-generic (clone (o))
            (list->vector (vector->list o)) ) ) ))

;;; Define a new toplevel with syntax-case as macroexpander.
;;; A small toplevel loop that uses the syntax-case package of Hieb
;;; and Dybvig (define-syntax is used throughout the book). Since many
;;; of the tests of the book also use the load function, this one
;;; should be redefined to expand with syntax-case first.

(define (start)
  (display "[C. Queinnec's book] SCM+Meroonet+syntax-case...")
  (newline)
  (set! *syntax-case-load-verbose?* #t)
  (set! load syntax-case-load)
  (interpreter
   "? " "= " #t
   (lambda (read print err)
     (set! tester-error   err)
     (set! meroonet-error err)
     (lambda ()
       (let* ((e (read))
              (r (catch-error (eval (expand-syntax e)))) )
         (if (pair? r)
             (print (car r))
             (err 'book-toplevel r) ) ) ) ) )
  (display " Ite LiSP est.")
  (newline)
  (exit 0) )

;;; Warp into the new toplevel.
(start)

;;; end of Init.scm
