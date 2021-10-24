;;; $Id: tester.scm,v 1.18 1997/12/07 16:22:44 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; or  Lisp In Small Pieces (Cambridge University Press).
;;; By Christian Queinnec <Christian.Queinnec@LIP6.fr>
;;; Newest version may be retrieved from:
;;;     http://www-spi.lip6.fr/~queinnec/WWW/LiSP.html
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;;                         *********************************************
;;;                          An engine to test interpreters or compilers
;;;                                    Christian Queinnec 
;;;                          \'Ecole Polytechnique  & INRIA-Rocquencourt
;;;                         *********************************************

;;; This file contains the basic facility to easily run an interpreter
;;; defining a language. It can ask the user for expressions to be 
;;; evaluated and print their results or take expressions from a file,
;;; evaluate them and compare their value to the expected value. If an 
;;; error occurs then the test suite is aborted. 

;;; A validation suite is a sequence of expressions followed by their
;;; expected results. The expression is evaluated in the language being
;;; tested then its result is compared to the expected result. 
;;; An example of a test suite is:
;;; /------------------------
;;; | (car '(a b))
;;; |    a
;;; | (car (list))
;;; |    ***      ; means that an error is expected
;;; | (oblist)
;;; |    ---     ; means that no error is expected but the value is unimportant
;;; It is an error of course to expect an error if no error occurrs.

;;; This package may use the tester-error function in case of an
;;; internal error. It is not defined here since errors are not
;;; portable. You must have a location to define that variable for
;;; some compilers. Two cases may trigger this error: a suite test
;;; missing an expecting result or a (toplevel) form returning an
;;; unprinted value. Other errors are caught within engine-tester.

;;; The testing engine can be parameterized in various ways. 
;;; Apart the engine itself, two functions are offered that ease its use.
;;; These are the `interpreter' and `suite-test' functions:

;;; This is the read function to use. It is possible to tailor this
;;; function to suit particular needs (for instance when generating
;;; tests instead of reading them out of a file).

(define tester-read read)

;;; This is an internal variable that, in real Scheme, may be true or
;;; false without perceivable difference. It does make a difference
;;; however in non compliant Scheme systems. If your Scheme only
;;; offers dynamic-extent continuations, you must set it to false (in
;;; that case, all continuations created by tester.scm will be used in
;;; their dynamic extent).  Alternatively, in Scheme->C which seems to
;;; have problems to garbage collect continuations, set it to true to
;;; only take one continuation and use it out of its dynamic extent.

(define tester-take-only-one-continuation #f)

;;; This is the call/cc to use. It is possible to use dynamic-extent
;;; continuations instead. For instance, in Bigloo, you may use:
;;;    (set! tester-call/cc (lambda (f)
;;;                           (bind-exit (k) (f k)) ))
;;; But don't forget to define tester-take-only-one-continuation to false.

(define tester-call/cc call/cc)

;;; The interpreter function takes four arguments:
;;;  -- an input prompt. This string will be printed whenever the 
;;;     interpreter wants to read an expression to evaluate.
;;;  -- an output prompt. Values are printed preceded by this string.
;;;  -- an error handler which is called whenever an error is detected.
;;;  -- a make-toplevel function that will return a thunk implementing
;;;     one interpreting step (read-eval-print).
;;;     make-toplevel may be roughly defined as
;;;       (lambda (read-exression display-value error-catcher) 
;;;         (lambda () (display-value (tested-eval (read-expression)))) )
;;;     The toplevel function will be repeatedly invoked from the 
;;;     interpreter. make-toplevel is invoked only once 
;;;     by the testing-engine with
;;;   == (read-expression): a function that reads an expression and echoes it
;;;      after printing the input-prompt. It returns the read expression.
;;;   == (display-value v): the function that prints a value preceded by
;;;      the output prompt. A new toplevel step is performed right after.
;;;   == (error-catcher message . culprits): a function that reports the error,
;;;      aborts the current computation and restarts a new cycle.
;;;    You must write your interpreter so that when it detects errors,
;;;    it calls this error-catcher function. You can have single error
;;;    function or trap the native error function of your particular
;;;    system.

(define (interpreter prompt-in          ; prompt to read expression
                     prompt-out         ; prompt preceding results
                     continue?          ; continue after unexpected error
                     make-toplevel )    ; toplevel generator
  ;; display the result of an evaluation
  (define (display-status status expected v)
    (case status
      ((unexpected-error)
       (newline)
       (display v)
       (display "  an unexpected ERROR occurs !!!")
       (newline)
       continue? )
      ((correct-result)
       (display prompt-out)
       (display v)
       (newline)
       #t )                             ; continue iteration
      (else #f) ) )                     ; stop iteration
  ;; starts toplevel
  (tester-call/cc
   (lambda (exit)                       ; exit when test suite is finished
     (engine-tester
      (lambda ()                        ; read expression
        (display prompt-in)
        (let ((e (tester-read)))
          (if (eof-object? e)
              (exit 'end) )
          e ) )
      (lambda () 'nothing)              ; read expected result (useless)
      (lambda (expected obtained)       ; compare expected and obtained results
        (cond ((eq? obtained '***) #f)
              ((eq? obtained '---) #f)
              (else #t) ) )
      display-status
      make-toplevel ) ) ) )

;;; suite-test is similar to the preceding one except that tests are taken
;;; from a file, possibly echoed on the console and checked to be correct. 
;;; The suite contains expressions followed by their expected result.
;;; The result of the evaluation is compared to this result, the 
;;; suite is aborted if an error occurs.

;;; suite-test takes six arguments:
;;;  -- a file-name: The file contains the expressions to be evaluated and
;;;     the expected results.
;;;  -- an input prompt
;;;  -- an output prompt
;;;  -- a boolean flag which governs if read expressions from the file are
;;;     echoed on the console.
;;;  -- a make-toplevel function that will return the toplevel function
;;;     make-toplevel may be roughly defined as
;;;       (lambda (test-read test-checker wrong) 
;;;         (lambda () (test-checker (tested-eval (test-read)))) )
;;;     The toplevel function will be repeatedly invoked from the 
;;;     interpreter. The arguments of make-toplevel are
;;;   == (test-read): the function that reads an expression, echoing it
;;;      after printing the input-prompt.
;;;   == (test-checker v): this function takes a value, reads the expected
;;;      result, compares them and if according, prints the value preceded by
;;;      the output prompt. A new toplevel is started after that.
;;;   == (wrong message . culprits): a function that reports the error,
;;;      aborts the current computation and restart a new cycle.
;;;  -- a (comparator) function that takes the obtained result and the
;;;     expected result and compares them yielding a boolean.
;;;     Very often, result-eval is just `equal?' but must
;;;     recognize the *** and --- items which meaning is "an error is
;;;     expected" or "an unimportant value (but no error)".

(define (suite-test file                ; the test suite
                    prompt-in           ; the prompt to read
                    prompt-out          ; the prompt to display
                    echo?               ; echo expressions ?
                    make-toplevel       ; a toplevel generator
                    compare )           ; how to compare results
  (let ((in (open-input-file file))
        (native-display display)
        (native-newline newline) )
    ;; Two small utilities to display things
    (define (display exp)
      (if echo? (native-display exp)) )
    (define (newline)
      (if echo? (native-newline)) )
    ;; Display the result of the test, return a boolean to indicate
    ;; whether the tests should continue or not.
    (define (display-status status expected v)
      (case status
        ((expected-error)
         (set! echo? #t)
         (display prompt-out)
         (display v)
         (display "  an ERROR was expected !!! ")
         (newline)
         #f )                           ; stop iteration
        ((error-occurred)
         (display " OK OK")
         (newline)
         #t )                           ; continue iteration
        ((unexpected-error)
         (newline)
         (display v)
         (set! echo? #t)
         (display "  an unexpected ERROR occured !!!")
         (newline)
         (display "  value expected: ")
         (display expected)
         (newline)
         #f )                           ; stop iteration
        ((correct-result)
         (display prompt-out)
         (display v)
         (display "  OK")
         (newline)
         #t )                           ; continue iteration
        ((incorrect-result)
         (set! echo? #t)
         (display prompt-out)
         (display v)
         (display "  ERROR !!!")
         (newline)
         (display "value expected:")
         (display expected)
         (newline)
         #f )                           ; stop iteration
        ((uninteresting-result)
         (display "  OK")
         (newline)
         #t )                           ; continue iteration
        (else (display "No such status")
              (newline)
              #f ) ) )                  ; stop iteration
    (tester-call/cc
     (lambda (exit)                     ; exit when test suite is finished
       (engine-tester 
        (lambda ()                      ; read test
          (let ((e (tester-read in)))
            (if (eof-object? e)
                (begin (close-input-port in) (exit 'done)) )
            (display prompt-in)
            (display e)
            (newline)
            e ) )
        (lambda ()                      ; read result
          (let ((expected (tester-read in)))
            (if (eof-object? expected)
                (tester-error "Missing expected result" expected) 
                expected ) ) )
        compare
        display-status
        make-toplevel ) ) ) ) )

;;; A test engine on top of which the two previous are written:
;;;  (read-test)    reads an expression to evaluate
;;;  (read-result)  reads the expected result
;;;  (compare expected obtained)  compares what was obtained from what
;;;                 was expected. The value of `expected' can also 
;;;                 be *** or ---
;;;  (display-status message expected obtained) displays the result of the
;;;                 test. It usually prints the result and a comment like `OK'.
;;;                 Testing is abandoned if display-status returns #f.
;;;  (make-toplevel read print error) returns a thunk implementing one step
;;;                 of the intepreter. 

(define (engine-tester read-test        ; read a test
                       read-result      ; read the expected result
                       compare          ; compare the two
                       display-status   ; display the comparison
                       make-toplevel )  ; make a toplevel
  (tester-call/cc 
   (lambda (abort)                      ; exit all tests
     (let ((resume #f))                 ; will be initialized below.
       ;; compare the result V with what was expected. If that
       ;; function is called then no error ocurred (unless *** is
       ;; given to it simulating an error internally caught).
       (define (check-result v)
         (let ((expected (read-result)))
           (if (cond ((compare expected v)
                      (display-status 'correct-result expected v) )
                     ((eq? expected '***)
                      (display-status 'expected-error expected v) )
                     ((eq? expected '---)
                      (display-status 'uninteresting-result expected v) )
                     (else 
                      (display-status 'incorrect-result expected v) ) )
               (resume #t)
               (abort #f) ) ) )
       ;; This function is called whenever an error is detected.
       (define (handle-exception msg . culprits)
         ;;(write `(handle-exception called))(newline) ; DEBUG
         (let ((expected (read-result))
               (v        (cons msg culprits)) )
           (if (cond ((eq? expected '***)
                      (display-status 'error-occurred expected v) )
                     (else 
                      (display-status 'unexpected-error expected v) ) )
               (resume #t)
               (abort #f) ) ) )
       (let ((toplevel (make-toplevel read-test 
                                      check-result 
                                      handle-exception )))
         ;; The goal is to call (toplevel) ever and ever but to ensure
         ;; that the continuation is correctly reset.
         (let loop ()
           (tester-call/cc 
            (lambda (k) 
              (if (and tester-take-only-one-continuation resume)
                  'nothing
                  (set! resume k) )
              (let ((r (toplevel)))
                ;; if this error is triggered, see note below.
                (tester-error "(toplevel) should not return!" r) ) ) )
           (loop) ) ) ) ) ) )

;;; Examples:
;;; Suppose you have written an interpreter called `evaluate', then the 
;;; following will start a toplevel loop. Errors detected in evaluate
;;; are supposed to call the `wrong' function.
;;;(define (scheme)
;;;  (interpreter "?? " " == " #t
;;;    (lambda (read print error)
;;;      (set! wrong error)   ;; Errors in the interpreter calls wrong
;;;      (lambda () (print (evaluate (read)))) ) ) )
;;; The problem is that errors in the underlying system are not caught.
;;; Suppose at that time to have something to trap errors, say catch-error
;;; as in Mac-Lisp (it returns the result in a pair or the string that names 
;;; the error if any), then you can write:
;;;(define (scheme)
;;;  (interpreter "?? " " == " #t
;;;    (lambda (read print error)
;;;      (set! wrong error)   ;; Errors in the interpreter calls wrong
;;;      (lambda () (let ((r (catch-error (evaluate (read)))))
;;;                   (if (pair? r) (print (car r))
;;;                       (error r) ) )) ) ) )

;;; NOTE: Both the print and error functions (in interpreter and
;;; suite-test) have a control effect. They restart a new toplevel
;;; iteration. So it is *important* not to forget to call them to
;;; reiterate the toplevel. If you return a value from toplevel
;;; without calling print or error, you'll get an internal error (ie
;;; an invocation of tester-error). [The reason lies with toplevel
;;; returning more than once in some concurrent interpreter I wrote].

;;; If you have a file containing a test suite, say suite.tst, then you 
;;; can try it with: 
;;;(define (test-scheme)
;;;  (suite-test "suite.tst"
;;;    "?? " "== " #t
;;;    (lambda (read print error)
;;;      (set! wrong error)
;;;      (lambda ()
;;;         (print (eval (read))) ) )
;;;    equal? ) )
;;; Another comparison function could be:
;;;    (lambda (expected obtained)
;;;      (cond ((or (eq? obtained '---)(eq? obtained '***))
;;;             (equal? expected obtained) )
;;;            (else (member obtained expected)) ) )
;;; Other suggestions: tests and results can be read from two different files.
;;; You can use other compare functions such as member, set-equal? or even
;;; use pattern-matching. Here are the two lastly mentioned comparators.

;;; Compares if sets X and Y have the same (with equal?) elements.

(define (set-equal? x y)
  (define (remove-one item list)
    (if (pair? list)
        (if (equal? item (car list))
            (cdr list)
            (cons (car list) (remove-one item (cdr list))) )
        '() ) )
  (if (pair? x)
      (and (member (car x) y)
           (set-equal? (cdr x) (remove-one (car x) y)) )
      (null? y) ) )

;;; Compares if the expression fits the pattern. Two special patterns exist: 
;;;     ?-   which accepts anything
;;;     ??-  which accepts a (possibly empty) sequence of anything.
;;; Otherwise comparisons are performed with equal?.

(define (naive-match pattern expression)
  (define (naive-match-list patterns expressions)
    (if (pair? patterns)
        (if (eq? (car patterns) '??-) ; accepts any sequence of things
            (or (naive-match-list (cdr patterns) expressions)
                (and (pair? expressions)
                     (naive-match-list patterns (cdr expressions)) ) )
            (and (pair? expressions)
                 (naive-match (car patterns) (car expressions))
                 (naive-match-list (cdr patterns) (cdr expressions)) ) )
        (naive-match patterns expressions) ) )
  (or (eq? pattern '?-)              ; accepts anything
      (if (pair? pattern)
          (naive-match-list pattern expression)
          (equal? pattern expression) ) ) )


;;; AGAIN A NOTE:
;;; To catch the errors of the underlying Scheme is difficult.

;;; This tester engine has been used since 1992 on a wide variety of
;;; interpreters, some of which are concurrent and/or return multiple
;;; results.

;;; end of tester.scm
