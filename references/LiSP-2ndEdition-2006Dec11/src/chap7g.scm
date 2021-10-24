;;; $Id: chap7g.scm,v 4.3 2006/11/27 14:05:47 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; From chap6b.scm. Make global modifiable environment incremental.

(define (compute-kind r n)
  (or (local-variable? r 0 n)
      (global-variable? g.current n)
      (global-variable? g.init n)
      (adjoin-global-variable! n) ) )

;;; Do not check if the global environment is large enough.
;;; The new variable is stored at the beginning of sg.current.
;;; No need to initialize sg.current!

(define (adjoin-global-variable! name)
  (let ((index (g.current-extend! name)))
    (cdr (car g.current)) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Separate compilation

(define (read-file filename)
  (call-with-input-file filename
    (lambda (in)
      (let gather ((e (read in))
                   (content '()) )
        (if (eof-object? e)
            (reverse content)
            (gather (read in) (cons e content)) ) ) ) ) )

(define (compile-file filename)
  (set! g.current '())
  (set! *quotations* '())
  (set! *dynamic-variables* '())
  (let* ((complete-filename (string-append filename ".scm"))
         (e               `(begin . ,(read-file complete-filename)))
         (code            (make-code-segment (meaning e r.init #t)))
         (global-names    (map car (reverse g.current)))
         (constants       (apply vector *quotations*))
         (dynamics        *dynamic-variables*)
         (ofilename       (string-append filename ".so")) )
    (write-result-file ofilename 
                       (list ";;; Bytecode object file for " 
                             complete-filename )
                       dynamics global-names constants code
                       (length (code-prologue)) ) ) )

(define (write-result-file ofilename comments dynamics global-names 
                           constants code entry )
  (call-with-output-file ofilename
    (lambda (out)
      (for-each (lambda (comment) (display comment out))
                comments )(newline out)(newline out)
      (display ";;; Dynamic variables" out)(newline out)
      (write dynamics out)(newline out)(newline out)
      (display ";;; Global modifiable variables" out)(newline out)
      (write global-names out)(newline out)(newline out)
      (display ";;; Quotations" out)(newline out)
      (write constants out)(newline out)(newline out)
      (display ";;; Bytecode" out)(newline out)
      (write code out)(newline out)(newline out)
      (display ";;; Entry point" out)(newline out)
      (write entry out) (newline out) ) ) )

;;; This function installs an object file and returns its entry point.
;;; The probe-file does not belong to Scheme.
(define (probe-file ofilename) #t)

(define (install-object-file! filename)
  (let ((ofilename (string-append filename ".so")))
    (if (probe-file ofilename)
        (call-with-input-file ofilename
          (lambda (in)
            (let* ((dynamics     (read in))
                   (global-names (read in))
                   (constants    (read in))
                   (code         (read in))
                   (entry        (read in)) )
              (close-input-port in)
              (relocate-globals! code global-names)
              (relocate-constants! code constants)
              (relocate-dynamics! code dynamics)
              (+ entry (install-code! code)) ) ) )
        (signal #f (list "No such file" ofilename)) ) ) )

(define (install-code! code)
  (let ((start (vector-length *code*)))
    (set! *code* (vector-append *code* code))
    start ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Relocations. Should be done in one pass for greater eficiency.

(define CHECKED-GLOBAL-REF-code 8)
(define GLOBAL-REF-code 7)
(define SET-GLOBAL!-code 27)

(define (relocate-globals! code global-names)
  (define (get-index name)
    (let ((where (memq name sg.current.names)))
      (if where (- (length where) 1)
          (begin (set! sg.current.names (cons name sg.current.names))
                 (get-index name) ) ) ) )
  (let ((code-size (vector-length code)))
    (let scan ((pc 0))
      (when (< pc code-size)
        (let ((instr (vector-ref code pc)))
          (when (or (= instr CHECKED-GLOBAL-REF-code)
                    (= instr GLOBAL-REF-code)
                    (= instr SET-GLOBAL!-code) )
            (let* ((i (vector-ref code (+ pc 1)))
                   (name (list-ref global-names i)) )
              (vector-set! code (+ pc 1) (get-index name)) ) )
          (scan (+ pc (instruction-size code pc))) ) ) ) )
  (let ((v (make-vector (length sg.current.names) undefined-value)))
    (vector-copy! sg.current v 0 (vector-length sg.current))
    (set! sg.current v) ) )

(define DYNAMIC-REF-code 240)
(define DYNAMIC-PUSH-code 242)

(define (relocate-dynamics! code dynamics)
  (for-each get-dynamic-variable-index dynamics)
  (let ((dynamics (reverse! dynamics))
        (code-size (vector-length code)) )
    (let scan ((pc 0))
      (when (< pc code-size)
        (let ((instr (vector-ref code pc)))
          (when (or (= instr DYNAMIC-REF-code)
                    (= instr DYNAMIC-PUSH-code) )
            (let* ((i (vector-ref code (+ pc 1)))
                   (name (list-ref dynamics (- i 1))) )
              (vector-set! code (+ pc 1) 
                           (get-dynamic-variable-index name) ) ) )
          (scan (+ pc (instruction-size code pc))) ) ) ) ) )

;;; Don't care to coalesce quotations (another day perhaps :)

(define CONSTANT-code 9)

(define (relocate-constants! code constants)
  (define n (vector-length *constants*))
  (let ((code-size (vector-length code)))
    (let scan ((pc 0))
      (when (< pc code-size)
        (let ((instr (vector-ref code pc)))
          (when (= instr CONSTANT-code)
            (let* ((i (vector-ref code (+ pc 1)))
                   (quotation (vector-ref constants i)) )
              (vector-set! code (+ pc 1) (+ n i)) ) )
          (scan (+ pc (instruction-size code pc))) ) ) ) )
  (set! *constants* (vector-append *constants* constants)) )

;;; rather inefficient !

(define (vector-append v1 v2)
  (apply vector (append (vector->list v1) (vector->list v2))) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; A function to dynamically load a file
;;; As the return pc is on top of the stack, just calls the freshly
;;; installed module, its last RETURN will return to the invoker of load.

(definitial load
  (let* ((arity 1)
         (arity+1 (+ arity 1)) )
    (make-primitive
     (lambda ()
       (if (= arity+1 (activation-frame-argument-length *val*))
           (let ((filename (activation-frame-argument *val* 0)))
             (set! *pc* (install-object-file! filename)) )
           (signal-exception 
            #t (list "Incorrect arity" 'load) ) ) ) ) ) )

;;; A function to get the value of a global variable by its name

(definitial global-value
  (let* ((arity 1)
         (arity+1 (+ arity 1)) )
    (define (get-index name)
      (let ((where (memq name sg.current.names)))
        (if where (- (length where) 1)
            (signal-exception 
             #f (list "Undefined global variable" name) ) ) ) )
    (make-primitive
     (lambda ()
       (if (= arity+1 (activation-frame-argument-length *val*))
           (let* ((name (activation-frame-argument *val* 0))
                  (i (get-index name)) )
             (set! *val* (global-fetch i))
             (when (eq? *val* undefined-value)
               (signal-exception 
                #f (list "Uninitialized variable" i) ) )
             (set! *pc* (stack-pop)) )
           (signal-exception 
            #t (list "Incorrect arity" 'global-value) ) ) ) ) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Builds an application. Concatenate and relocate files.

(define (build-application application-name ofilename . ofilenames)
  (set! sg.current.names    '())
  (set! *dynamic-variables* '())
  (set! sg.current          (vector))
  (set! *constants*         (vector))
  (set! *code*              (vector))
  (let install ((filenames (cons ofilename ofilenames))
                (entry-points '()) )
    (if (pair? filenames)
        (let ((ep (install-object-file! (car filenames))))
          (install (cdr filenames) (cons ep entry-points)) )
        (write-result-file application-name
                           (cons ";;; Bytecode application containing "
                                 (cons ofilename ofilenames) )
                           *dynamic-variables*
                           sg.current.names
                           *constants*
                           *code*
                           entry-points ) ) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; rename exported variables. It is even possible to swap variables as in:
;;; (build-application-renaming-variables 
;;;     "tmp.si/na.out" "tmp.si/a.out" '((fib fact) (fact fib)) )

(define (build-application-renaming-variables 
         new-application-name application-name substitutions )
  (if (probe-file application-name)
      (call-with-input-file application-name
        (lambda (in)
          (let* ((dynamics     (read in))
                 (global-names (read in))
                 (constants    (read in))
                 (code         (read in))
                 (entries      (read in)) )
            (close-input-port in)
            (write-result-file 
             new-application-name
             (list ";;; renamed variables from " application-name)
             dynamics 
             (let sublis ((global-names global-names))
               (if (pair? global-names)
                   (cons (let ((s (assq (car global-names) 
                                        substitutions )))
                           (if (pair? s) (cadr s)
                               (car global-names) ) )
                         (sublis (cdr global-names)) )
                   global-names ) )
             constants
             code
             entries ) ) ) )
      (signal #f (list "No such file" application-name)) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Run an application ie install all files then run them in order.
;;; It can run a built application or a single compiled file.

(define (run-application stack-size filename)
  (if (probe-file filename)
      (call-with-input-file filename
        (lambda (in)
          (let* ((dynamics     (read in))
                 (global-names (read in))
                 (constants    (read in))
                 (code         (read in))
                 (entry-points (read in)) )
            (close-input-port in)
            (set! sg.current.names    global-names)
            (set! *dynamic-variables* dynamics)
            (set! sg.current (make-vector (length sg.current.names) 
                                          undefined-value ))
            (set! *constants*         constants)
            (set! *code*              (vector))
            (install-code! code)
            (set! *env*               sr.init)
            (set! *stack*             (make-vector stack-size))
            (set! *stack-index*       0)
            (set! *val*               'anything)
            (set! *fun*               'anything)
            (set! *arg1*              'anything)
            (set! *arg2*              'anything)
            (push-dynamic-binding 
             0 (list (make-primitive (lambda () 
                                       (show-exception)
                                       (*exit* 'aborted) ))) )
            (stack-push 1)                        ; pc for FINISH
            (if (pair? entry-points)
                (for-each stack-push entry-points)
                (stack-push entry-points) ) )
          (set! *pc* (stack-pop))
          (call/cc (lambda (exit)
                     (set! *exit* exit)
                     (run) )) ) )
      (static-wrong "No such file" filename) ) )

;;; This one will be included in the book.
(define (show-exception)
  (display "Panic error.")(newline)
  (abort) )

(define (show-exception)
  (format #t "
		>>>>>>>>>>>>>>>>>>RunTime PANIC<<<<<<<<<<<<<<<<<<<<<<<<<
		~A~%" (activation-frame-argument *val* 1) )
  (set! *debug* #t)
  (show-registers "Panic error:") 
  (format #t "Global resources:
	Constants: ~A
	Names of global variables: ~A
	Names of dynamic variables: ~A
" *constants* sg.current.names *dynamic-variables* )
  (display (disassemble *code*)) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Tests

(define (test-scheme7g file)
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
     (setup-wrong-functions error)
     (lambda ()
       (call-with-output-file "tmp.si/tmp.scm"
         (lambda (out) (write (skip-read) out)) )
       (compile-file "tmp.si/tmp")
       (set! *debug* #f)
       (run-tested-application 100 "tmp.si/tmp.so")
       (check *val*) ) )
   equal? ) )

(define (run-tested-application stack-size filename)
  (if (probe-file filename)
      (call-with-input-file filename
        (lambda (in)
          (let* ((dynamics     (read in))
                 (global-names (read in))
                 (constants    (read in))
                 (code         (read in))
                 (entry-points (read in)) )
            (close-input-port in)
            (set! sg.current.names    global-names)
            (set! *dynamic-variables* dynamics)
            (set! sg.current (make-vector (length sg.current.names) 
                                          undefined-value ))
            (set! *constants*         constants)
            (set! *code*              (vector))
            (install-code! code)
            (set! *env*               sr.init)
            (set! *stack*             (make-vector stack-size))
            (set! *stack-index*       0)
            (set! *val*               'anything)
            (set! *fun*               'anything)
            (set! *arg1*              'anything)
            (set! *arg2*              'anything)
            (push-dynamic-binding 0 (list 'base-error-handler-primitive))
            (stack-push 1)                        ; pc for FINISH
            (if (pair? entry-points)
                (for-each stack-push entry-points)
                (stack-push entry-points) ) )
          (set! *pc* (stack-pop))
          (call/cc (lambda (exit)
                     (set! *exit* exit)
                     (let* ((errprim (make-primitive (lambda () 
                                                       (show-exception)
                                                       (wrong 'aborted) )))
                            (handlers (search-exception-handlers)) )
                       (set-car! handlers errprim) )
                     (run) )) ) )
      (static-wrong "No such file" filename) ) )

(define *tests-to-skip*
  '( xyzzy
     (set! xyzzy 3)
     ((lambda (x y) xyzzy) 1 2)
    ) )

;;; end of chap7g.scm
