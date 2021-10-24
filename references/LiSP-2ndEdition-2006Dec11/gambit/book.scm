;;; $Id: book.scm,v 1.6 1996/02/11 14:09:30 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; This file customizes the Gambit interpreter from Marc Feeley in order
;;; to run the source files of the book. I did not try to compile it yet.
;;; Under Unix, start gsi as:
;;;          gsi '(include "gambit/book.scm")'

;;; This variable is used in chap8k.scm to determine the underlying
;;; Scheme interpreter.

(define book-interpreter-support 'gsi)

;;; Missing functions:

(define call/cc call-with-current-continuation)
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

;;; Name the Un*x ports (no flush function ?)

(define stdout-port (current-output-port))
(define stderr-port ##stderr)

;;; plist are present but not under the same functions.  Unfortunately
;;; Gambit-C 2.2b seems to have a buf on Plists of certain symbols. So
;;; I took the naive, slow but safe implementation of src/chap2d.

(define putprop put)
(define getprop get)

(let ((properties '()))
  (set! putprop
        (lambda (symbol key value)
          (let ((plist (assq symbol properties)))
            (if (pair? plist)
                (let ((couple (assq key (cdr plist))))
                  (if (pair? couple)
                      (set-cdr! couple value)
                      (set-cdr! plist (cons (cons key value)
                                            (cdr plist) )) ) )
                (let ((plist (list symbol (cons key value))))
                  (set! properties (cons plist properties)) ) ) )
          value ) )
  (set! getprop
        (lambda (symbol key)
          (let ((plist (assq symbol properties)))
            (if (pair? plist)
                (let ((couple (assq key (cdr plist))))
                  (if (pair? couple)
                      (cdr couple)
                      #f ) )
                #f ) ) ) ) )

;;; erase predefined get and put.
(set! get getprop)
(set! put putprop)

;;; internal runtime

(define get-internal-run-time runtime)

;;; No flush function on buffers.

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

;;; property lists are already present.

;;; pp is already present but not format. but format needs internal functions
;;; of pp so redefine pp.

(include "gambit/pp.scm")

(include "gambit/format.scm")

;;; Loads syntax-case V2.0.  

(define error-hook error)

(include "gambit/compat.ss")
(include "gambit/hooks.gsi")
(include "gambit/output.ss")
(include "gambit/init.ss")
(include "gambit/expand.bb")
(include "gambit/macro-defs.bb")

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Calls to this function might be generated by syntax-case.scm

(define (list* . args)
  (if (pair? args)
      (if (pair? (cdr args))
          (cons (car args) (apply list* (cdr args)))
          (car args) )
      (quote ()) ) )

;;; Two macros that I frequently use:
;;; Since, the macroexpander of the current load in progress is still not
;;; aware of the change of macroexpansion algorithm, invoke expand-syntax
;;; directly to register them and define them also for Gambit.

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

(include "src/tester.scm")

;;; Load Meroonet. Meroonet defines three macros with
;;; define-meroonet-macro. These macros are not needed by the rest of
;;; this file, so it is sufficient to register them with syntax-case.
;;; But, instead of syntax-case-loading the file, I only load it with 
;;; a definition of define-meroonet-macro that defines it for Dybvig.

(define-macro (define-meroonet-macro call . body)
  (define (meroonet-make-expander call body x)
    `(begin
       ;; define the Dybvig expander as a regular function:
       (putprop ',(car call) 'expander
         (lambda (,x)
           ;; x receives a syntax-object and not a form
           (define (strip x) (vector-ref x 1))
           ;; Don't check arity, apply directly
           (apply (lambda ,(cdr call) . ,body)
                  (cdr (strip ,x)) ) ) )
       ;; register the Dybvig macro at load-time:
       (expand-syntax
        '(define-syntax ,(car call) (getprop ',(car call) 'expander)) ) ) )
  (meroonet-make-expander call body (gensym)) )

(include "meroonet/meroonet.scm")

;;; The test-driver should try to catch errors of the underlying Scheme 
;;; system. This is non-portable and difficult in many implementations. If
;;; you do not succeed writing it, you can still run the programs of the book
;;; but you will not be able to run all the test-suites since some tests 
;;; (for instance in meroonet/oo-tests.scm) require errors to be caught
;;; when signalled by list-tail with a non-numeric second argument.

(expand-syntax
 '(define-syntax catch-error
    (syntax-rules ()
      ((catch-error forms ...)
       (protected-eval 
        (lambda () 
          (list (begin forms ...)) ) ) ) ) ) )

(define (make-new-error exit)
  (lambda (string . culprits)
    (newline stderr-port)
    (display "== ERROR == " stderr-port)
    (display string stderr-port)
    (newline stderr-port)
    (for-each (lambda (o) (bounded-display o stderr-port))
              culprits )
    (newline stderr-port)
    (exit '**error**) ) )

(define (protected-eval thunk)
  (call/cc (lambda (exit)
             (##dynamic-let (list (cons '##SIGNAL-CATCHER 
                                        (make-new-error exit) ))
                            thunk ) )) )
;;; Test: (protected-eval (lambda () (car #t)))

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
     (set! meroonet-error err)
     (set! tester-error   err)
     (set! error-hook     err)
     (set! support-error  err)
     (lambda ()
       (set-gc-report #t)
       (##dynamic-let (list (cons '##SIGNAL-CATCHER err))
          (lambda ()
            (let* ((e (read))
                   (g (expand-syntax e))
                   (r (eval g))
                   )
              (check r) ) ) ) ) )
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
  (display "[C. Queinnec's book] Gambit+Meroonet+syntax-case...")
  (newline)
  (set! *syntax-case-load-verbose?* #t)
  (set! load syntax-case-load)
  (interpreter
   "? " "= " #t
   (lambda (read print err)
     (set! tester-error   err)
     (set! meroonet-error err)
     (set! error-hook     err)
     (set! support-error  err)
     (set! ##user-interrupt
           (lambda ()
             (newline stderr-port)
             (display "*** INTERRUPT" stderr-port)
             (newline stderr-port)
             (err '***) ) )
     (lambda ()
       (##dynamic-let (list (cons '##SIGNAL-CATCHER err))
          (lambda ()
            (let* ((e (read))
                   (r (eval (expand-syntax e))) )
              (print r) ) ) ) ) ) )
  (display " Ite LiSP est.")
  (newline)
  (##quit) )

;;; Warp into the new toplevel.
(start)

;;; end of book.scm
