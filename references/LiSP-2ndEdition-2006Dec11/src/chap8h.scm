;;; $Id: chap8h.scm,v 4.3 2006/11/24 18:40:48 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Add to the bytecode compiler the export special form to reify
;;; environments.  eval is a regular binary function now taking an
;;; expression and an environment.

(define (meaning e r tail?)
  (if (atom? e)
      (if (symbol? e) (meaning-reference e r tail?)
                      (meaning-quotation e r tail?) )
      (case (car e)
        ((quote)  (meaning-quotation (cadr e) r tail?))
        ((lambda) (meaning-abstraction (cadr e) (cddr e) r tail?))
        ((if)     (meaning-alternative (cadr e) (caddr e) (cadddr e) r tail?))
        ((begin)  (meaning-sequence (cdr e) r tail?))
        ((set!)   (meaning-assignment (cadr e) (caddr e) r tail?))
        ((bind-exit) (meaning-bind-exit (caadr e) (cddr e) r tail?))
        ((dynamic) (meaning-dynamic-reference (cadr e) r tail?))
        ((dynamic-let) (meaning-dynamic-let (car (cadr e))
                                            (cadr (cadr e))
                                            (cddr e) r tail? ))
        ((monitor) (meaning-monitor (cadr e) (cddr e) r tail?))
        ((export)  (meaning-export (cdr e) r tail?))                      ; \modified
        (else      (meaning-application (car e) (cdr e) r tail?)) ) ) )

;;; Redefine a little the hierarchy of environments.

(define-class environment Object
  ( next ) )

(define-class activation-frame environment
  ( (* argument)
    ) )
(define-class reified-environment Object
  ( sr r ) )

(define-method (show (a reified-environment) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "[Reified env r=" stream)
    (show (reified-environment-r a) stream)
    (display ", sr=" stream)
    (show (reified-environment-sr a) stream)
    (display "]" stream) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Change the representation of the compile-time environment
;;; r = ( (name (local i . j)) ...)

(define (r-extend* r n*)
  (let ((old-r (bury-r r 1)))
    (let scan ((n* n*)(i 0))
      (cond ((pair? n*) (cons (list (car n*) `(local 0 . ,i))
                              (scan (cdr n*) (+ i 1)) ))
            ((null? n*) old-r)
            (else (cons (list n* `(local 0 . ,i)) old-r)) ) ) ) )

(define (checked-r-extend* r n*)
  (let ((old-r (bury-r r 1)))
    (let scan ((n* n*)(i 0))
      (cond ((pair? n*) (cons (list (car n*) `(checked-local 0 . ,i))
                              (scan (cdr n*) (+ i 1)) ))
            ((null? n*) old-r) ) ) ) )

(define (bury-r r offset)
  (map (lambda (d)
         (let ((name (car d))
               (type (car (cadr d))) )
           (case type
             ((local checked-local) 
              (let* ((addr (cadr d))
                     (i (cadr addr))
                     (j (cddr addr)) )
                `(,name (,type ,(+ i offset) . ,j) . ,(cddr d)) ) )
             (else d) ) ) )
       r ) )

(define r.init '())

(define (compute-kind r n)
  (or (let ((var (assq n r)))
        (and (pair? var) (cadr var)) )
      (global-variable? g.current n)
      (global-variable? g.init n)
      (adjoin-global-variable! n) ) )

;;; Extract from the compile-time environment, the addresses of the
;;; sole variables that are needed.

(define (extract-addresses n* r)
  (if (null? n*) r
      (let scan ((n* n*))
        (if (pair? n*)
            (cons (list (car n*) (compute-kind r (car n*)))
                  (scan (cdr n*)) )
            '() ) ) ) )

;;; Must retrofit these to recognize checked-local variables.

(define (meaning-reference n r tail?)
  (let ((kind (compute-kind r n)))
    (if kind
        (case (car kind)
          ((checked-local)
           (let ((i (cadr kind))
                 (j (cddr kind)) )
             (CHECKED-DEEP-REF i j) ) )
          ((local)
           (let ((i (cadr kind))
                 (j (cddr kind)) )
             (if (= i 0)
                 (SHALLOW-ARGUMENT-REF j)
                 (DEEP-ARGUMENT-REF i j) ) ) )
          ((global)
           (let ((i (cdr kind)))
             (CHECKED-GLOBAL-REF i) ) )
          ((predefined)
           (let ((i (cdr kind)))
             (PREDEFINED i) ) ) )
        (static-wrong "No such variable" n) ) ) )

(define (meaning-assignment n e r tail?) 
  (let ((m (meaning e r #f))
        (kind (compute-kind r n)) )
    (if kind
        (case (car kind)
          ((local checked-local)
           (let ((i (cadr kind))
                 (j (cddr kind)) )
             (if (= i 0)
                 (SHALLOW-ARGUMENT-SET! j m)
                 (DEEP-ARGUMENT-SET! i j m) ) ) )
          ((global)
           (let ((i (cdr kind)))
             (GLOBAL-SET! i m) ) )
          ((predefined)
           (static-wrong "Immutable predefined variable" n) ) )
        (static-wrong "No such variable" n) ) ) )

(define (CHECKED-DEEP-REF i j) (list 253 i j))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The special form export
;;; (export) exports all the local environment 
;;; while (export n...) only exports these variables.

(define (meaning-export n* r tail?)
  (unless (every? symbol? n*)
          (static-wrong "Incorrect variables" n*) )
  (append (CONSTANT (extract-addresses n* r)) 
          (CREATE-1ST-CLASS-ENV) ) )

(define (create-first-class-environment r sr)
  (set! *val* (make-reified-environment sr r)) )

(define (CREATE-1ST-CLASS-ENV) (list 254))

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; A binary function named eval. It takes an environment as second
;;; argument. It finds in it a compilation environment and a runtime
;;; environment.

(definitial eval/b
  (let* ((arity 2)
         (arity+1 (+ arity 1)) )
    (make-primitive
     (lambda ()
       (if (= arity+1 (activation-frame-argument-length *val*))
           (let ((exp (activation-frame-argument *val* 0))
                 (env (activation-frame-argument *val* 1)) )
             (if (program? exp)
                 (if (reified-environment? env)
                     (compile-and-evaluate exp env)
                     (signal-exception 
                      #t (list "Not an environment" env) ) )
                 (signal-exception 
                  #t (list "Illegal program" exp) ) ) )
           (signal-exception 
            #t (list "Incorrect arity" 'eval/b) ) ) ) ) ) )

(define (compile-and-evaluate v env)
  (let ((r (reified-environment-r env))
        (sr (reified-environment-sr env)) )
    (set! *env* sr)
    (set! *pc* (compile-on-the-fly v r)) ) )

;;; Compile program v within environment r, install resulting code and
;;; return its entry point. (same as in chap8d.scm)

(define (compile-on-the-fly v r)
  (set! g.current '())
  (for-each g.current-extend! sg.current.names)
  (set! *quotations* (vector->list *constants*))
  (set! *dynamic-variables* *dynamic-variables*)
  (let ((code (apply vector (append (meaning v r #f) (RETURN)))))
    (set! sg.current.names (map car (reverse g.current)))
    (let ((v (make-vector (length sg.current.names) undefined-value)))
      (vector-copy! sg.current v 0 (vector-length sg.current))
      (set! sg.current v) )
    (set! *constants* (apply vector *quotations*))
    (set! *dynamic-variables* *dynamic-variables*)
    (install-code! code) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Enrich an environment. This is a function that yields a new environment.

(definitial enrich 
  (let* ((arity 1)
         (arity+1 (+ arity 1)) )
    (make-primitive
     (lambda ()
       (if (>= (activation-frame-argument-length *val*) arity+1)
           (let ((env (activation-frame-argument *val* 0)))
             (listify! *val* 1)
             (if (reified-environment? env)
                 (let* ((names (activation-frame-argument *val* 1))
                        (len (- (activation-frame-argument-length 
                                 *val* ) 
                                2 ))
                        (r (reified-environment-r env))
                        (sr (reified-environment-sr env))
                        (frame (allocate-activation-frame 
                                (length names) )) )
                   (set-activation-frame-next! frame sr)
                   (do ((i (- len 1) (- i 1)))
                       ((< i 0))
                     (set-activation-frame-argument! 
                      frame i undefined-value ) )
                   (unless (every? symbol? names)
                     (signal-exception 
                      #f (list "Incorrect variable names" names ) ) )
                   (set! *val* (make-reified-environment 
                                frame 
                                (checked-r-extend* r names) ))
                   (set! *pc* (stack-pop)) )
                 (signal-exception 
                  #t (list "Not an environment" env) ) ) )
           (signal-exception 
            #t (list "Incorrect arity" 'enrich) ) ) ) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Access an environment. Pay attention not to use compute-kind since
;;; it automatically creates variables.

(definitial variable-value 
  (let* ((arity 2)
         (arity+1 (+ arity 1)) )
    (make-primitive
     (lambda ()
       (if (= (activation-frame-argument-length *val*) arity+1)
           (let ((name (activation-frame-argument *val* 0))
                 (env (activation-frame-argument *val* 1)) )
             (if (reified-environment? env)
                 (if (symbol? name)
                     (let* ((r (reified-environment-r env))
                            (sr (reified-environment-sr env))
                            (kind 
                             (or (let ((var (assq name r)))
                                   (and (pair? var) (cadr var)) )
                                 (global-variable? g.current name)
                                 (global-variable? g.init name) ) ) )
                       (variable-value-lookup kind sr)
                       (set! *pc* (stack-pop)) )
                     (signal-exception 
                      #f (list "Not a variable name" name) ) )
                 (signal-exception 
                  #t (list "Not an environment" env) ) ) )
           (signal-exception #t (list "Incorrect arity" 
                                      'variable-value )) ) ) ) ) )

(define (variable-value-lookup kind sr)
  (if (pair? kind)
      (case (car kind)
        ((checked-local)
         (let ((i (cadr kind))
               (j (cddr kind)) )
           (set! *val* (deep-fetch sr i j))
           (when (eq? *val* undefined-value)
             (signal-exception 
              #t (list "Uninitialized local variable") ) ) ) )
        ((local)
         (let ((i (cadr kind))
               (j (cddr kind)) )
           (set! *val* (if (= i 0)
                           (activation-frame-argument sr j)
                           (deep-fetch sr i j) )) ) )
        ((shadowable)
         (let ((i (cadr kind))
               (j (cddr kind)) )
           (shadowable-fetch sr i j) ) )
        ((global)
         (let ((i (cdr kind)))
           (set! *val* (global-fetch i))
           (when (eq? *val* undefined-value)
             (signal-exception #t 
               (list "Uninitialized global variable") ) ) ) )
        ((predefined)
         (let ((i (cdr kind)))
           (set! *val* (predefined-fetch i)) ) ) )
      (signal-exception #f (list "No such variable")) ) )

(definitial set-variable-value!
  (let* ((arity 3)
         (arity+1 (+ arity 1)) )
    (make-primitive
     (lambda ()
       (if (= (activation-frame-argument-length *val*) arity+1)
           (let ((name (activation-frame-argument *val* 0))
                 (env (activation-frame-argument *val* 1))
                 (value (activation-frame-argument *val* 2)) )
             (if (reified-environment? env)
                 (if (symbol? name)
                     (let* ((r (reified-environment-r env))
                            (sr (reified-environment-sr env))
                            (kind (or (let ((var (assq name r)))
                                        (and (pair? var) (cadr var)) )
                                      (global-variable? g.current name)
                                      (global-variable? g.init name) )) )
                       (variable-value-update! kind sr value)
                       (set! *pc* (stack-pop)) )
                     (signal-exception #f (list "Not a variable name" name)) )
                 (signal-exception #t (list "Not an environment" env)) ) )
           (signal-exception #t (list "Incorrect arity" 
                                      'set-variable-value! )) ) ) ) ) )

(define (variable-value-update! kind sr value)
  (if (pair? kind)
      (case (car kind)
        ((checked-local)
         (let ((i (cadr kind))
               (j (cddr kind)) )
           (deep-update! sr i j value) ) )
        ((local)
         (let ((i (cadr kind))
               (j (cddr kind)) )
           (if (= i 0)
               (set-activation-frame-argument! sr j value)
               (deep-update! sr i j value) ) ) )
        ((shadowable)
         (let ((i (cadr kind))
               (j (cddr kind)) )
           (shadowable-update! sr i j value) ) )
        ((global)
         (let ((i (cdr kind)))
           (global-update! i value) ) )
        ((predefined)
         (signal-exception #f (list "Immutable global variable")) ) )
      (signal-exception #f (list "No such variable")) ) )

;;; To test whether a variable exists must not create it.

(definitial variable-defined?
  (let* ((arity 2)
         (arity+1 (+ arity 1)) )
    (make-primitive
     (lambda ()
       (if (= (activation-frame-argument-length *val*) arity+1)
           (let ((name (activation-frame-argument *val* 0))
                 (env (activation-frame-argument *val* 1)) )
             (if (reified-environment? env)
                 (if (symbol? name)
                     (let* ((r (reified-environment-r env))
                            (sr (reified-environment-sr env)) )
                       (set! *val* 
                             (if (or (let ((var (assq name r)))
                                       (and (pair? var) (cadr var)) )
                                     (global-variable? g.current name)
                                     (global-variable? g.init name) )
                                 #t #f ) )
                       (set! *pc* (stack-pop)) )
                     (signal-exception 
                      #f (list "Not a variable name" name) ) )
                 (signal-exception 
                  #t (list "Not an environment" env) ) ) )
           (signal-exception #t (list "Incorrect arity" 
                                      'variable-defined? )) ) ) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; destructuring closures.

(definitial procedure->environment
  (let* ((arity 1)
         (arity+1 (+ arity 1)) )
    (make-primitive
     (lambda ()
       (if (>= (activation-frame-argument-length *val*) arity+1)
           (let ((proc (activation-frame-argument *val* 0)))
             (if (closure? proc)
                 (let* ((pc (closure-code proc))
                        (r (vector-ref 
                            *constants*
                            (vector-ref *code* (- pc 1)) )) )
                   (set! *val* (make-reified-environment 
                                (closure-closed-environment proc)
                                r ))
                   (set! *pc* (stack-pop)) )
                 (signal-exception 
                  #f (list "Not a procedure" proc) ) ) )
           (signal-exception 
            #t (list "Incorrect arity" 'enrich) ) ) ) ) ) )

(definitial procedure->definition
  (let* ((arity 1)
         (arity+1 (+ arity 1)) )
    (make-primitive
     (lambda ()
       (if (>= (activation-frame-argument-length *val*) arity+1)
           (let ((proc (activation-frame-argument *val* 0)))
             (if (closure? proc)
                 (let ((pc (closure-code proc)))
                   (set! *val* (vector-ref 
                                *constants*
                                (vector-ref *code* (- pc 3)) ))
                   (set! *pc* (stack-pop)) )
                 (signal-exception 
                  #f (list "Not a procedure" proc) ) ) )
           (signal-exception 
            #t (list "Incorrect arity" 'enrich) ) ) ) ) ) )

;;; Analyzers must now preserve the compile-time lexical environment
;;; as well as the body of the abstraction.

(define (meaning-fix-abstraction n* e+ r tail?)
  (let* ((arity (length n*))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence e+ r2 #t)) )
    (REFLECTIVE-FIX-CLOSURE m+ arity `(lambda ,n* . ,e+) r) ) )

(define (meaning-dotted-abstraction n* n e+ r tail?)
  (let* ((arity (length n*))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence e+ r2 #t)) )
    (REFLECTIVE-NARY-CLOSURE m+ arity `(lambda ,n* . ,e+) r) ) )

;;; We must use explicit constant and not predefined0 or other shortcuts.
;;; We insert references to date within code (but jump over them).

(define (REFLECTIVE-FIX-CLOSURE m+ arity definition r)
  (let* ((the-function (append (ARITY=? (+ arity 1)) (EXTEND-ENV)
                               m+  (RETURN) ))
         (the-env (append (EXPLICIT-CONSTANT definition) 
                          (EXPLICIT-CONSTANT r) ))
         (the-goto (GOTO (+ (length the-env) 
                            (length the-function) ))) )
    (append (CREATE-CLOSURE (+ (length the-goto) (length the-env)))
            the-goto the-env the-function ) ) )

(define (REFLECTIVE-NARY-CLOSURE m+ arity definition r)
  (let* ((the-function (append (ARITY>=? (+ arity 1)) (PACK-FRAME! arity)
                               (EXTEND-ENV) m+ (RETURN) ))
         (the-env (append (EXPLICIT-CONSTANT definition) 
                          (EXPLICIT-CONSTANT r) ))
         (the-goto (GOTO (+ (length the-env) (length the-function)))) )
    (append (CREATE-CLOSURE (+ (length the-goto) (length the-env)))
            the-goto the-env the-function ) ) )

;;; end of chap8h.scm
