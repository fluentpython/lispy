;;; $Id: chap6d.scm,v 4.4 2006/11/27 14:04:24 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;;                      Threaded interpreter.
;;; Environment is held by a global variable. This is bad for //ism.
;;; Continuation are now implicit and call/cc is a magical operator.
;;; Also try to introduce combinators as much as possible.
;;; Closures are explicitely represented.

(define *env* '())

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Determine the nature of a variable.
;;; Three different answers. Or the variable is local (ie appears in R)
;;; then return     (LOCAL index . depth)
;;; global (ie created by the user) then return
;;;                 (GLOBAL . index)
;;; or predefined (and immutable) then return
;;;                 (PREDEFINED . index)

(define (compute-kind r n)
  (or (local-variable? r 0 n)
      (global-variable? g.current n)
      (global-variable? g.init n) ) )

(define (local-variable? r i n)
  (and (pair? r)
       (let scan ((names (car r))
                  (j 0) )
         (cond ((pair? names) 
                (if (eq? n (car names))
                    `(local ,i . ,j)
                    (scan (cdr names) (+ 1 j)) ) )
               ((null? names)
                (local-variable? (cdr r) (+ i 1) n) )
               ((eq? n names) `(local ,i . ,j)) ) ) ) )


(define (global-variable? g n)
  (let ((var (assq n g)))
    (and (pair? var)
         (cdr var) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Representation of local environments, they contain the values of
;;; the local variables (but global and predefined variables).
;;; Runtime environment or, activation frames, are represented by 
;;; vectors (named v*). They have the following structure:
;;;           +------------+
;;;           | next       |  ---> next V*
;;;           | argument 0 |  value of the first argument
;;;           | argument 1 |  value of the second argument
;;;           .            .
;;;           | free slot  |  Free slot for nary variable
;;;           +------------+
;;; The number of arguments can be extracted from the size of the
;;; activation frame.

;;; A direct implementation with inlined vectors is approximatively 
;;; 7 times faster under sci.

(define-class environment Object 
  ( next ) )

(define-class activation-frame environment
  ( (* argument) ) )

(define (sr-extend* sr v*)
  (set-environment-next! v* sr)
  v* )

(define sr.init '())

;;; Fetch the value of the Ith argument of the Jth frame.

(define (deep-fetch sr i j)
  (if (= i 0)
      (activation-frame-argument sr j)
      (deep-fetch (environment-next sr) (- i 1) j) ) )

(define (deep-update! sr i j v)
  (if (= i 0)
      (set-activation-frame-argument! sr j v)
      (deep-update! (environment-next sr) (- i 1) j v) ) )

;;; R is the static representation of the runtime local environment.
;;; It is represented by a list of list of variables (the classical
;;; rib cage). 

(define (r-extend* r n*)
  (cons n* r) )

(define r.init '())

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; User-defined global environment definition. This environment is
;;; initially completely empty and can be extended by the user.
;;; It actually tolerates only 100 new global variables.

;;; G.CURRENT represents the `static' user-defined global environment. 
;;; It is represented by the list of the symbols naming these global
;;; variables. Their values are held in the SG.CURRENT vector.

(define g.current '())

(define sg.current (make-vector 100))

(define (g.current-extend! n)
  (let ((level (length g.current)))
    (set! g.current 
          (cons (cons n `(global . ,level)) g.current) )
    level ) )

(define (global-fetch i)
  (vector-ref sg.current i) )

(define (global-update! i v)
  (vector-set! sg.current i v) )

(define (g.current-initialize! name)
  (let ((kind (compute-kind r.init name)))
    (if kind
        (case (car kind)
          ((global)
           (vector-set! sg.current (cdr kind) undefined-value) )
          (else (static-wrong "Wrong redefinition" name)) )
        (let ((index (g.current-extend! name)))
          (vector-set! sg.current index undefined-value) ) ) )
  name )

;;; This tag is used in the value cell of uninitialized variables.

(define undefined-value (cons 'undefined 'value))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Predefined global environment definition. This global environment
;;; is immutable. G.INIT represents the static predefined global
;;; environment and is represented by the list of the symbols naming
;;; these global variables. Their values are held in the SG.INIT vector.

(define g.init '())

(define sg.init (make-vector 100))

(define (predefined-fetch i)
  (vector-ref sg.init i) )

(define (g.init-extend! n)
  (let ((level (length g.init)))
    (set! g.init
          (cons (cons n `(predefined . ,level)) g.init) )
    level ) )

;;; Add that value is associated to name in the predefined global environment.

(define (g.init-initialize! name value)
  (let ((kind (compute-kind r.init name)))
    (if kind
        (case (car kind)
          ((predefined)
           (vector-set! sg.init (cdr kind) value) )
          (else (static-wrong "Wrong redefinition" name)) )
        (let ((index (g.init-extend! name)))
          (vector-set! sg.init index value) ) ) )
  name )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Describe a predefined value.
;;; The description language only represents primitives with their arity:
;;;          (FUNCTION address . variables-list)
;;; with variables-list := () | (a) | (a b) | (a b c)
;;; Only the structure of the VARIABLES-LIST is interesting (not the
;;; names of the variables). ADDRESS is the address of the primitive
;;; to use when inlining an invokation to it. This address is
;;; represented by a Scheme procedure.

(define desc.init '())

(define (description-extend! name description)
  (set! desc.init 
        (cons (cons name description) desc.init) )
  name )

;;; Return the description or #f if absent.

(define (get-description name)
  (let ((p (assq name desc.init)))
    (and (pair? p) (cdr p)) ) )
        
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Representation of functions. A redefinition with inlined vectors
;;; for more speed.

(define-class closure Object
  ( code
    closed-environment
    ) )

(define (invoke f v*)
  (if (closure? f)
      ((closure-code f) v* (closure-closed-environment f))
      (wrong "Not a function" f) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Combinators

(define (SHALLOW-ARGUMENT-REF j)
  (lambda () (activation-frame-argument *env* j)) )

(define (PREDEFINED i)
  (lambda () (predefined-fetch i)) )

(define (DEEP-ARGUMENT-REF i j)
  (lambda () (deep-fetch *env* i j)) )

(define (SHALLOW-ARGUMENT-SET! j m)
  (lambda () (set-activation-frame-argument! *env* j (m))) )

(define (DEEP-ARGUMENT-SET! i j m)
  (lambda () (deep-update! *env* i j (m))) )

(define (GLOBAL-REF i)
  (lambda () (global-fetch i)) )

;;; Note that we lost the name of the variable, it must be retrieved
;;; from elsewhere.   TOBEDONE

(define (CHECKED-GLOBAL-REF i)
  (lambda () 
    (let ((v (global-fetch i)))
      (if (eq? v undefined-value)
          (wrong "Uninitialized variable")
          v ) ) ) )

(define (GLOBAL-SET! i m)
  (lambda () (global-update! i (m))) )

(define (CONSTANT value)
  (lambda () value) )

(define (ALTERNATIVE m1 m2 m3)
  (lambda () (if (m1) (m2) (m3))) )

(define (SEQUENCE m m+)
  (lambda () (m) (m+)) )

(define (TR-FIX-LET m* m+)
  (lambda ()
    (set! *env* (sr-extend* *env* (m*)))
    (m+) ) )

(define (FIX-LET m* m+)
  (lambda ()
    (set! *env* (sr-extend* *env* (m*)))
    (let ((result (m+)))
      (set! *env* (environment-next *env*))
      result ) ) )

(define (CALL0 address)
  (lambda () (address)) )

(define (CALL1 address m1)
  (lambda () (address (m1))) )

(define (CALL2 address m1 m2)
  (lambda () (let ((v1 (m1))) 
               (address v1 (m2)) )) )

(define (CALL3 address m1 m2 m3)
  (lambda () (let* ((v1 (m1))
                    (v2 (m2)) )
               (address v1 v2 (m3)) )) )

(define (FIX-CLOSURE m+ arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (define (the-function v* sr)
        (if (= (activation-frame-argument-length v*) arity+1)
            (begin (set! *env* (sr-extend* sr v*))
                   (m+) )
            (wrong "Incorrect arity") ) )
      (make-closure the-function *env*) ) ) )

(define (NARY-CLOSURE m+ arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (define (the-function v* sr)
        (if (>= (activation-frame-argument-length v*) arity+1)
            (begin 
              (listify! v* arity)
              (set! *env* (sr-extend* sr v*))
              (m+) )
            (wrong "Incorrect arity") ) )
      (make-closure the-function *env*) ) ) )

(define (TR-REGULAR-CALL m m*) 
  (lambda ()
    (let ((f (m)))
      (invoke f (m*)) ) ) )

(define (REGULAR-CALL m m*)
  (lambda ()
    (let* ((f (m))
           (v* (m*))
           (sr *env*)
           (result (invoke f v*)) )
      (set! *env* sr)
      result ) ) )

(define (STORE-ARGUMENT m m* rank)
  (lambda ()
    (let* ((v (m))
           (v* (m*)) )
      (set-activation-frame-argument! v* rank v)
      v* ) ) )

(define (CONS-ARGUMENT m m* arity)
  (lambda ()
    (let* ((v (m))
           (v* (m*)) )
      (set-activation-frame-argument! 
       v* arity (cons v (activation-frame-argument v* arity)) )
      v* ) ) )

(define (ALLOCATE-FRAME size)
  (let ((size+1 (+ size 1)))
    (lambda ()
      (allocate-activation-frame size+1) ) ) )

(define (ALLOCATE-DOTTED-FRAME arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (let ((v* (allocate-activation-frame arity+1)))
        (set-activation-frame-argument! v* arity '())
        v* ) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The threaded interpreter.
;;; E is the expression to evaluate
;;; SR is the representation of the local lexical environment
;;; TAIL? is a boolean that indicates if E is a terminal call (also
;;; means whether the *env* register should be restored or not).

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
        (else     (meaning-application (car e) (cdr e) r tail?)) ) ) )

(define (meaning-reference n r tail?)
  (let ((kind (compute-kind r n)))
    (if kind
        (case (car kind)
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

(define (meaning-quotation v r tail?)
  (CONSTANT v) )

(define (meaning-alternative e1 e2 e3 r tail?)
  (let ((m1 (meaning e1 r #f))
        (m2 (meaning e2 r tail?))
        (m3 (meaning e3 r tail?)) )
    (ALTERNATIVE m1 m2 m3) ) )

(define (meaning-assignment n e r tail?) 
  (let ((m (meaning e r #f))
        (kind (compute-kind r n)) )
    (if kind
        (case (car kind)
          ((local)
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

(define (meaning-sequence e+ r tail?)
  (if (pair? e+)
      (if (pair? (cdr e+))
          (meaning*-multiple-sequence (car e+) (cdr e+) r tail?)
          (meaning*-single-sequence (car e+) r tail?) )
      (static-wrong "Illegal syntax: (begin)") ) )

(define (meaning*-single-sequence e r tail?) 
  (meaning e r tail?) )

(define (meaning*-multiple-sequence e e+ r tail?)
  (let ((m1 (meaning e r #f))
        (m+ (meaning-sequence e+ r tail?)) )
    (SEQUENCE m1 m+) ) )

(define (meaning-abstraction nn* e+ r tail?)
  (let parse ((n* nn*)
              (regular '()) )
    (cond
     ((pair? n*) (parse (cdr n*) (cons (car n*) regular)))
     ((null? n*) (meaning-fix-abstraction nn* e+ r tail?))
     (else       (meaning-dotted-abstraction 
                  (reverse regular) n* e+ r tail? )) ) ) )

(define (meaning-fix-abstraction n* e+ r tail?)
  (let* ((arity (length n*))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence e+ r2 #t)) )
    (FIX-CLOSURE m+ arity) ) )

(define (meaning-dotted-abstraction n* n e+ r tail?)
  (let* ((arity (length n*))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence e+ r2 #t)) )
    (NARY-CLOSURE m+ arity) ) )

;;; Application meaning.

(define (meaning-application e e* r tail?)
  (cond ((and (symbol? e)
              (let ((kind (compute-kind r e)))
                (and (pair? kind)
                     (eq? 'predefined (car kind))
                     (let ((desc (get-description e)))
                       (and desc
                            (eq? 'function (car desc))
                            (or (= (length (cddr desc)) (length e*))
                                (static-wrong 
                                 "Incorrect arity for primitive" e )
                                ) ) ) ) ) )
         (meaning-primitive-application e e* r tail?) )
        ((and (pair? e)
              (eq? 'lambda (car e)) )
         (meaning-closed-application e e* r tail?) )
        (else (meaning-regular-application e e* r tail?)) ) )

;;; Parse the variable list to check the arity and detect wether the
;;; abstraction is dotted or not.

(define (meaning-closed-application e ee* r tail?)
  (let ((nn* (cadr e)))
    (let parse ((n* nn*)
                (e* ee*)
                (regular '()) )
      (cond
       ((pair? n*) 
        (if (pair? e*)
            (parse (cdr n*) (cdr e*) (cons (car n*) regular))
            (static-wrong "Too less arguments" e ee*) ) )
       ((null? n*)
        (if (null? e*)
            (meaning-fix-closed-application 
             nn* (cddr e) ee* r tail? )
            (static-wrong "Too much arguments" e ee*) ) )
       (else (meaning-dotted-closed-application 
              (reverse regular) n* (cddr e) ee* r tail? )) ) ) ) )

(define (meaning-fix-closed-application n* body e* r tail?)
  (let* ((m* (meaning* e* r (length e*) #f))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence body r2 tail?)) )
    (if tail? (TR-FIX-LET m* m+) 
        (FIX-LET m* m+) ) ) )

(define (meaning-dotted-closed-application n* n body e* r tail?)
  (let* ((m* (meaning-dotted* e* r (length e*) (length n*) #f))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence body r2 tail?)) )
    (if tail? (TR-FIX-LET m* m+)
        (FIX-LET m* m+) ) ) )

;;; Handles a call to a predefined primitive. The arity is already checked.
;;; The optimization is to avoid the allocation of the activation frame.
;;; These primitives never change the *env* register nor have control effect.

(define (meaning-primitive-application e e* r tail?)
  (let* ((desc (get-description e))
         ;; desc = (function address . variables-list)
         (address (cadr desc))
         (size (length e*)) )
    (case size
      ((0) (CALL0 address))
      ((1) 
       (let ((m1 (meaning (car e*) r #f)))
         (CALL1 address m1) ) )
      ((2) 
       (let ((m1 (meaning (car e*) r #f))
             (m2 (meaning (cadr e*) r #f)) )
         (CALL2 address m1 m2) ) )
      ((3) 
       (let ((m1 (meaning (car e*) r #f))
             (m2 (meaning (cadr e*) r #f))
             (m3 (meaning (caddr e*) r #f)) )
         (CALL3 address m1 m2 m3) ) )
      (else (meaning-regular-application e e* r tail?)) ) ) )

;;; In a regular application, the invocation protocol is to call the
;;; function with an activation frame and a continuation: (f v* k).

(define (meaning-regular-application e e* r tail?)
  (let* ((m (meaning e r #f))
         (m* (meaning* e* r (length e*) #f)) )
    (if tail? (TR-REGULAR-CALL m m*) (REGULAR-CALL m m*)) ) )

(define (meaning* e* r size tail?)
  (if (pair? e*)
      (meaning-some-arguments (car e*) (cdr e*) r size tail?)
      (meaning-no-argument r size tail?) ) )

(define (meaning-dotted* e* r size arity tail?)
  (if (pair? e*)
      (meaning-some-dotted-arguments (car e*) (cdr e*) 
                                     r size arity tail? )
      (meaning-no-dotted-argument r size arity tail?) ) )

(define (meaning-some-arguments e e* r size tail?)
  (let ((m (meaning e r #f))
        (m* (meaning* e* r size tail?))
        (rank (- size (+ (length e*) 1))) )
    (STORE-ARGUMENT m m* rank) ) )

(define (meaning-some-dotted-arguments e e* r size arity tail?)
  (let ((m (meaning e r #f))
        (m* (meaning-dotted* e* r size arity tail?))
        (rank (- size (+ (length e*) 1))) )
    (if (< rank arity) (STORE-ARGUMENT m m* rank)
        (CONS-ARGUMENT m m* arity) ) ) )

(define (meaning-no-argument r size tail?)
  (ALLOCATE-FRAME size) )

(define (meaning-no-dotted-argument r size arity tail?)
  (ALLOCATE-DOTTED-FRAME arity) )

;;; Gather into a list all arguments from arity+1 to the end of the
;;; activation frame and store this list into the arity+1th slot.

(define (listify! v* arity)
  (let loop ((index (- (activation-frame-argument-length v*) 1))
             (result '()) )
    (if (= arity index)
        (set-activation-frame-argument! v* arity result)
        (loop (- index 1)
              (cons (activation-frame-argument v* (- index 1))
                    result ) ) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Global environment initializers.

;;; Definitial allows to redefine immutable global variables. Useful
;;; when debugging interactively.

(define-syntax definitial
  (syntax-rules ()
    ((definitial name value)
     (g.init-initialize! 'name value) ) ) )

(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value 0)
     (defprimitive0 name value) )
    ((defprimitive name value 1)
     (defprimitive1 name value) )
    ((defprimitive name value 2)
     (defprimitive2 name value) )
    ((defprimitive name value 3)
     (defprimitive3 name value) ) ) )    

(define-syntax defprimitive1
  (syntax-rules ()
    ((defprimitive1 name value)
     (definitial name
       (letrec ((arity+1 (+ 1 1))
                (behavior
                 (lambda (v* sr)
                   (if (= (activation-frame-argument-length v*) 
                          arity+1 )
                       (value (activation-frame-argument v* 0))
                       (wrong "Incorrect arity" 'name) ) ) ) )
         (description-extend! 'name `(function ,value a))
         (make-closure behavior sr.init) ) ) ) ) )
  
(define-syntax defprimitive2
  (syntax-rules ()
    ((defprimitive2 name value)
     (definitial name
       (letrec ((arity+1 (+ 2 1))
                (behavior
                 (lambda (v* sr)
                   (if (= (activation-frame-argument-length v*)
                          arity+1 )
                       (value (activation-frame-argument v* 0) 
                              (activation-frame-argument v* 1) )
                       (wrong "Incorrect arity" 'name) ) ) ) )
         (description-extend! 'name `(function ,value a b))
         (make-closure behavior sr.init) ) ) ) ) )

;;; Define a location in the user global environment.

(define-syntax defvariable
  (syntax-rules ()
    ((defvariable name)
     (g.current-initialize! 'name) ) ) )

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

;;; We do not need to save the register *env* since call/cc is not a
;;; primitive (it is defined by definitial and not by defprimitive)
;;; and non-primitive invokations are regularly handled.

(definitial call/cc
  (let* ((arity 1)
         (arity+1 (+ arity 1)) )
    (make-closure
     (lambda (v* sr)
       (if (= arity+1 (activation-frame-argument-length v*))
           (call/cc
            (lambda (k)
              (invoke 
               (activation-frame-argument v* 0)
               (let ((frame (allocate-activation-frame (+ 1 1))))
                 (set-activation-frame-argument! 
                  frame 0
                  (make-closure
                   (lambda (values r)
                     (if (= (activation-frame-argument-length values)
                            arity+1 )
                         (k (activation-frame-argument values 0))
                         (wrong "Incorrect arity" 'continuation) ) )
                   sr.init ) )
                 frame ) ) ) )
           (wrong "Incorrect arity" 'call/cc) ) )
     sr.init ) ) )

(definitial apply
  (let* ((arity 2)
         (arity+1 (+ arity 1)) )
    (make-closure
     (lambda (v* sr)
       (if (>= (activation-frame-argument-length v*) arity+1)
           (let* ((proc (activation-frame-argument v* 0))
                  (last-arg-index
                   (- (activation-frame-argument-length v*) 2) )
                  (last-arg 
                   (activation-frame-argument v* last-arg-index) )
                  (size (+ last-arg-index (length last-arg)))
                  (frame (allocate-activation-frame size)) )
             (do ((i 1 (+ i 1)))
                 ((= i last-arg-index))
               (set-activation-frame-argument! 
                frame (- i 1) (activation-frame-argument v* i) ) )
             (do ((i (- last-arg-index 1) (+ i 1))
                  (last-arg last-arg (cdr last-arg)) )
                 ((null? last-arg))
               (set-activation-frame-argument! 
                frame i (car last-arg) ) )
             (invoke proc frame) )
           (wrong "Incorrect arity" 'apply) ) )
     sr.init ) ) )

(definitial list ((NARY-CLOSURE (SHALLOW-ARGUMENT-REF 0) 0)))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Some free global locations:

(defvariable x)
(defvariable y)
(defvariable z)
(defvariable a)
(defvariable b)
(defvariable c)
(defvariable foo)
(defvariable bar)
(defvariable hux)
(defvariable fib)
(defvariable fact)
(defvariable visit)
(defvariable length)
(defvariable primes)


;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Testing

(define (chapter63-interpreter)
  (define (toplevel)
    (set! *env* sr.init)
    (display ((meaning (read) r.init #t)))
    (toplevel) )
  (toplevel) )

;;; Preserve the current modifiable global environment (containing a,
;;; b, foo, fact, fib etc.) All tests will be compiled in that environment.

(define original.g.current
  (let ((g g.current))
    (lambda () g) ) )

;;; This variant produces a table of symbols.

(define sg.current.names (list 'foo))

(define (stand-alone-producer e)
  (set! g.current (original.g.current))
  (let* ((m (meaning e r.init #t))
         (size (length g.current))
         (global-names (map car (reverse g.current))) )
    (lambda ()
      (set! sg.current (make-vector size undefined-value))
      (set! sg.current.names global-names)
      (set! *env* sr.init)
      (m) ) ) )

(define (CHECKED-GLOBAL-REF+ i)
  (lambda () 
    (let ((v (global-fetch i)))
      (if (eq? v undefined-value)
          (wrong "Uninitialized variable" 
                 (list-ref sg.current.names i) )
          v ) ) ) )  

;;; this one requires to close the name of the variables that must be
;;; checked. To use it you must also change meaning-reference that calls it.

(define (CHECKED-GLOBAL-REF- i n)
  (lambda () 
    (let ((v (global-fetch i)))
      (if (eq? v undefined-value)
          (wrong "Uninitialized variable" n)
          v ) ) ) ) 

;;; retrofit for tests.
(set! CHECKED-GLOBAL-REF CHECKED-GLOBAL-REF+)

(define (scheme6d)
  (interpreter 
   "Scheme? "  
   "Scheme= " 
   #t
   (lambda (read print error)
     (set! wrong error)
     (set! static-wrong error)
     (lambda ()
       (set! *env* sr.init)
       (print ((stand-alone-producer (read)))) ) ) ) )

(define (test-scheme6d file)
  (suite-test 
   file 
   "Scheme? " 
   "Scheme= "
   #t
   (lambda (read check error)
     (set! wrong error)
     (set! static-wrong error)
     (lambda ()
       (check ((stand-alone-producer (read)))) ) )
   equal? ) )

;;; Pay attention to tail-rec in Scheme->C.

(define (bench6d factor e)
  (let ((start (get-internal-run-time))
        (m (meaning e r.init #t)) )
    (let loop ((factor factor))
      (set! *env* sr.init)
      (let ((v (m)))
        (let ((duration (- (get-internal-run-time) start)))
          (when (<= factor 1)
            (display (list duration v))
            (newline) ) ) )
      (if (> factor 1)
          (loop (- factor 1)) ) ) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The following code use 
;;; pp to pretty-print expressions,
;;; and eval for a local hack (should be made of macros instead).

(define combinator-names
  '( SHALLOW-ARGUMENT-REF
     PREDEFINED
     DEEP-ARGUMENT-REF
     SHALLOW-ARGUMENT-SET!
     DEEP-ARGUMENT-SET!
     GLOBAL-REF
     CHECKED-GLOBAL-REF
     GLOBAL-SET!
     CONSTANT
     ALTERNATIVE
     SEQUENCE
     TR-FIX-LET
     FIX-LET
     CALL0
     CALL1
     CALL2
     CALL3
     FIX-CLOSURE
     NARY-CLOSURE
     TR-REGULAR-CALL
     REGULAR-CALL
     STORE-ARGUMENT
     CONS-ARGUMENT
     ALLOCATE-FRAME
     ALLOCATE-DOTTED-FRAME ) )

(define install-regular-combinators
  (let ((originals (map eval combinator-names)))
    (lambda () 
      (for-each (lambda (old-value name)
                  (eval `(set! ,name ',old-value)) )
                originals
                combinator-names ) ) ) )

(define (install-disassembling-combinators)
  (for-each (lambda (name)
              (eval `(set! ,name (lambda args (,name . ,args)))) )
            combinator-names ) )

(define (disassemble e)
  (install-disassembling-combinators)
  (pp (meaning e r.init #t))
  (install-regular-combinators)
  (newline) )

;;; (disassemble '(lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
;;; (disassemble '(lambda (n) (if (= n 0) 1 (* (fact (- n 1)) n))))

;;; end of chap6d.scm
