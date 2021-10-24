;;; $Id: chap4a.scm,v 4.7 2006/12/03 10:54:47 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; The interpreter of chapter 4.

;;; Every value is encoded by a closure with message-passing style.

;;; Variable names
;;; Program       e et ef ec
;;; Environment   r
;;; Store         s ss sss ssss 
;;; Continuation  k kk
;;; Value         v vv
;;; Identifier    n
;;; Constant      c
;;; starred variables represent list of what represent the variables.

(define (evaluate e r s k)
  (if (atom? e) 
      (if (symbol? e) (evaluate-variable e r s k)
          (evaluate-quote e r s k) )
      (case (car e)
        ((quote)  (evaluate-quote (cadr e) r s k))
        ((if)     (evaluate-if (cadr e) (caddr e) (cadddr e) r s k))
        ((begin)  (evaluate-begin (cdr e) r s k))
        ((set!)   (evaluate-set! (cadr e) (caddr e) r s k))
        ((lambda) (evaluate-lambda (cadr e) (cddr e) r s k))
        (else     (evaluate-application (car e) (cdr e) r s k)) ) ) )

(define (evaluate-if ec et ef r s k)
  (evaluate ec r s
    (lambda (v ss)
      (evaluate ((v 'boolify) et ef) r ss k) ) ) )

;;; This amnesic alternative forgets about the store after the condition 
;;; is evaluated and revert to the store that was active when the 
;;; alternative was entered.
(define (evaluate-amnesic-if ec et ef r s k)
  (evaluate ec r s
    (lambda (v ss)
      (evaluate ((v 'boolify) et ef) r s   ;\relax{\tt s} $\not=$ {\tt ss}!
                k ) ) ) ) 

(define (evaluate-begin e* r s k)
  (if (pair? (cdr e*))
      (evaluate (car e*) r s 
        (lambda (void ss)
          (evaluate-begin (cdr e*) r ss k) ) )
      (evaluate (car e*) r s k) ) )

(define (evaluate-variable n r s k)
  (k (s (r n)) s) ) 

(define (evaluate-set! n e r s k)
  (evaluate e r s
    (lambda (v ss)
      (k v (update ss (r n) v)) ) ) )

;;; This assignment returns the value that is overwritten.
;;; Two variants might exist whether the old value is saved before or
;;; after the value that will be assigned is evaluated.
(define (new1-evaluate-set! n e r s k)
  (evaluate e r s
    (lambda (v ss)
      (k (ss (r n)) (update ss (r n) v)) ) ) )

(define (new2-evaluate-set! n e r s k)
  (evaluate e r s
    (lambda (v ss)
      (k (s (r n)) (update ss (r n) v)) ) ) )

(define (evaluate-application e e* r s k)
  (define (evaluate-arguments e* r s k)
    (if (pair? e*)
        (evaluate (car e*) r s
          (lambda (v ss)
            (evaluate-arguments (cdr e*) r ss
              (lambda (v* sss)
                (k (cons v v*) sss) ) ) ) )
        (k '() s) ) )
  (evaluate e r s
    (lambda (f ss)
      (evaluate-arguments e* r ss 
        (lambda (v* sss)
          (if (eq? (f 'type) 'function)
              ((f 'behavior) v* sss k)
              (wrong "Not a function" (car v*)) ) ) ) ) ) )

(define (evaluate-lambda n* e* r s k)
  (allocate 1 s
    (lambda (a* ss)
      (k (create-function 
          (car a*)
          (lambda (v* s k)
            (if (= (length n*) (length v*))
                (allocate (length n*) s
                 (lambda (a* ss)
                   (evaluate-begin e*  
                                   (update* r n* a*)
                                   (update* ss a* v*)
                                   k ) ) )
                (wrong "Incorrect arity") ) ) )
         ss ) ) ) )

;;; A la Fortran, non recursive functions
(define (evaluate-ftn-lambda n* e* r s k)
  (allocate (+ 1 (length n*)) s
    (lambda (a* ss)
      (k (create-function 
          (car a*)
          (lambda (v* s k)
            (if (= (length n*) (length v*))
                (evaluate-begin e*  
                                (update* r n* (cdr a*))
                                (update* s (cdr a*) v*)
                                k )
                (wrong "Incorrect arity") ) ) )
         ss ) ) ) )

;;; This one takes care of dotted variables.

(define (evaluate-nlambda n* e* r s k)
  (define (arity n*)
    (cond ((pair? n*) (+ 1 (arity (cdr n*))))
          ((null? n*) 0)
          (else       1) ) )
  (define (update-environment r n* a*)
    (cond 
     ((pair? n*) 
      (update-environment (update r (car n*) (car a*))
                          (cdr n*)
                          (cdr a*) ) )
     ((null? n*) r)
     (else (update r n* (car a*))) ) )
  (define (update-store s a* v* n*)
    (cond ((pair? n*) 
           (update-store (update s (car a*) (car v*))
                         (cdr a*) (cdr v*) (cdr n*) ) )
          ((null? n*) s)
          (else (allocate-list v* s (lambda (v ss) 
                                      (update ss (car a*) v) ))) ) )
  (allocate 1 s
    (lambda (a* ss)
      (k (create-function
          (car a*) 
          (lambda (v* s k)
            (if (compatible-arity? n* v*)
                (allocate (arity n*) s
                 (lambda (a* ss)
                   (evaluate-begin e*  
                                   (update-environment r n* a*)
                                   (update-store ss a* v* n*)
                                   k ) ) )
                (wrong "Incorrect arity") ) ) )
         ss ) ) ) )

(define (compatible-arity? n* v*)
  (cond ((pair? n*) (and (pair? v*) 
                         (compatible-arity? (cdr n*) (cdr v*)) ))
        ((null? n*) (null? v*))
        ((symbol? n*) #t) ) )

(define (r.init id)
  (wrong "No binding for" id) )

(define (update s a v)
  (lambda (aa)
    (if (eqv? a aa) v (s aa)) ) )

(define (update* s a* v*)
  ;; (assume (= (length a*) (length v*)))
  (if (pair? a*)
      (update* (update s (car a*) (car v*)) (cdr a*) (cdr v*))
      s ) )

(define (allocate n s q)
  (if (> n 0)
      (let ((a (new-location s)))
        (allocate (- n 1)
                  (expand-store a s)
                  (lambda (a* ss)
                    (q (cons a a*) ss) ) ) )
      (q '() s) ) ) 

(define (expand-store high-location s)
  (update s 0 high-location) )

(define (new-location s)
  (+ 1 (s 0)) )

(define s.init
  (expand-store 0 (lambda (a) (wrong "No such address" a))) )

;;; The initial value of these global mutable variables
(define r.global r.init)
(define s.global s.init)

(define (chapter4-interpreter)
  (define (toplevel s)
    (evaluate (read)
              r.global 
              s
              (lambda (v ss)
                (display (transcode-back v ss))
                (toplevel ss) ) ) )
  (toplevel s.global) )

(define (evaluate-quote c r s k)
  (transcode c s k) )

(define *shared-memo-quotations* '())

(define evaluate-memo-quote
  (lambda (c r s k)
    (let ((couple (assoc c *shared-memo-quotations*)))
      (if (pair? couple)
          (k (cdr couple) s)
          (transcode c s (lambda (v ss)
                           (set! *shared-memo-quotations*
                                 (cons (cons c v) 
                                       *shared-memo-quotations* ) )
                           (k v ss) )) ) ) ) )


(define (evaluate-immutable-quote c r s k)
  (immutable-transcode c s k) )

(define (immutable-transcode c s q)
  (cond
   ((null? c)    (q the-empty-list s))
   ((pair? c)
    (immutable-transcode 
     (car c) s (lambda (a ss)
                 (immutable-transcode 
                  (cdr c) ss (lambda (d sss)
                               (allocate-immutable-pair 
                                a d sss q ) ) ) ) ) )
   ((boolean? c) (q (create-boolean c) s))
   ((symbol? c)  (q (create-symbol c) s))
   ((string? c)  (q (create-string c) s))
   ((number? c)  (q (create-number c) s)) ) )

(define (create-immutable-pair a d)
  (lambda (msg)
    (case msg
      ((type)    'pair)
      ((boolify) (lambda (x y) x))
      ((set-car) (lambda (s v) (wrong "Immutable pair")))
      ((set-cdr) (lambda (s v) (wrong "Immutable pair")))
      ((car)     a)
      ((cdr)     d) ) ) )

(define (allocate-immutable-pair a d s q)
  (allocate 2 s
   (lambda (a* ss) 
     (q (create-immutable-pair (car a*) (cadr a*))
        (update (update ss (car a*) a) (cadr a*) d) ) ) ) )


(define the-empty-list
  (lambda (msg)
    (case msg
      ((type)    'null)
      ((boolify) (lambda (x y) x)) ) ) )

(define (transcode v s q)
  (cond
   ((null? c)    (q the-empty-list s))
   ((boolean? c) (q (create-boolean c) s))
   ((symbol? c)  (q (create-symbol c) s))
   ((string? c)  (q (create-string c) s))
   ((number? c)  (q (create-number c) s))
   ((pair? c)
    (transcode (car c) 
               s
               (lambda (a ss)
                 (transcode (cdr c)
                            ss
                            (lambda (d sss)
                              (allocate-pair a d sss q) ) ) ) ) ) ) ) 

(define (create-function tag behavior)
  (lambda (msg)
    (case msg
      ((type)     'function)
      ((boolify)  (lambda (x y) x))
      ((tag)      tag)
      ((behavior) behavior) ) ) )

(define (create-symbol v)
  (lambda (msg)
    (case msg
      ((type)    'symbol)
      ((name)    v)
      ((boolify) (lambda (x y) x)) ) ) )

(define (create-string v)
  (lambda (msg)
    (case msg
      ((type)    'string)
      ((chars)    v)
      ((boolify) (lambda (x y) x)) ) ) )

(define (create-number v)
  (lambda (msg)
    (case msg
      ((type)    'number)
      ((value)   v)
      ((boolify) (lambda (x y) x)) ) ) )

(define (transcode-back v s)
  (case (v 'type)
    ((null)     '())
    ((boolean)  ((v 'boolify) #t #f))
    ((symbol)   (v 'name))
    ((string)   (v 'chars))
    ((number)   (v 'value))
    ((pair)     (cons (transcode-back (s (v 'car)) s)
                      (transcode-back (s (v 'cdr)) s) ) )
    ((function) v) ; why not ?
    (else       (wrong "Unknown type" (v 'type))) ) )

(define (create-boolean value)
  (let ((combinator (if value (lambda (x y) x) (lambda (x y) y))))
    (lambda (msg)
      (case msg
        ((type)    'boolean)
        ((boolify) combinator) ) ) ) )

(define (create-pair a d)
  (lambda (msg)
    (case msg
      ((type)    'pair)
      ((boolify) (lambda (x y) x))
      ((set-car) (lambda (s v) (update s a v)))
      ((set-cdr) (lambda (s v) (update s d v)))
      ((car)     a)
      ((cdr)     d) ) ) )

(define (allocate-pair a d s q)
  (allocate 2 s
   (lambda (a* ss) 
     (q (create-pair (car a*) (cadr a*))
        (update (update ss (car a*) a) (cadr a*) d) ) ) ) )

(define (allocate-list v* s q)
  (define (consify v* q)
    (if (pair? v*)
        (consify (cdr v*) (lambda (v ss)
                            (allocate-pair (car v*) v ss q) ))
        (q the-empty-list s) ) )
  (consify v* q) )

;;; Backpatch some of the variants
(set! evaluate-lambda evaluate-nlambda)
(set! evaluate-set!   new1-evaluate-set!)
(set! evaluate-set!   new2-evaluate-set!)
(set! evaluate-quote  evaluate-memo-quote)
(set! evaluate-quote  evaluate-immutable-quote)

;;; the library

(define-syntax definitial
  (syntax-rules ()
   ((definitial name value)
    (allocate 1 s.global 
     (lambda (a* ss)
       (set! r.global (update r.global 'name (car a*)))
       (set! s.global (update ss (car a*) value)) ) ) ) ) )

(definitial t (create-boolean #t))

(definitial f (create-boolean #f))

(definitial nil the-empty-list)

(define-syntax defprimitive
  (syntax-rules ()
   ((defprimitive name value arity)
    (definitial name
      (allocate 1 s.global
       (lambda (a* ss)
         (set! s.global (expand-store (car a*) ss))
         (create-function
          (car a*) 
          (lambda (v* s k)
            (if (= arity (length v*))
                (value v* s k)
                (wrong "Incorrect arity" 'name) ) ) ) ) ) ) ) ) )

(defprimitive < 
  (lambda (v* s k)
    (if (and (eq? ((car v*) 'type) 'number)
             (eq? ((cadr v*) 'type) 'number) )
        (k (create-boolean (< ((car v*) 'value) ((cadr v*) 'value))) s)
        (wrong "< require numbers") ) )
  2 )

(defprimitive >
  (lambda (v* s k)
    (if (and (eq? ((car v*) 'type) 'number)
             (eq? ((cadr v*) 'type) 'number) )
        (k (create-boolean (> ((car v*) 'value) ((cadr v*) 'value))) s)
        (wrong "> require numbers") ) )
  2 )

(defprimitive <=
  (lambda (v* s k)
    (if (and (eq? ((car v*) 'type) 'number)
             (eq? ((cadr v*) 'type) 'number) )
        (k (create-boolean (<= ((car v*) 'value) 
                               ((cadr v*) 'value) ))
           s )
        (wrong "<= require numbers") ) )
  2 )

(defprimitive =
  (lambda (v* s k)
    (if (and (eq? ((car v*) 'type) 'number)
             (eq? ((cadr v*) 'type) 'number) )
        (k (create-boolean (= ((car v*) 'value) ((cadr v*) 'value))) s)
        (wrong "= require numbers") ) )
  2 )

(defprimitive +
  (lambda (v* s k)
    (if (and (eq? ((car v*) 'type) 'number)
             (eq? ((cadr v*) 'type) 'number) )
        (k (create-number (+ ((car v*) 'value) ((cadr v*) 'value))) s)
        (wrong "+ require numbers") ) )
  2 )

(defprimitive -
  (lambda (v* s k)
    (if (and (eq? ((car v*) 'type) 'number)
             (eq? ((cadr v*) 'type) 'number) )
        (k (create-number (- ((car v*) 'value) ((cadr v*) 'value))) s)
        (wrong "- require numbers") ) )
  2 )

(defprimitive *
  (lambda (v* s k)
    (if (and (eq? ((car v*) 'type) 'number)
             (eq? ((cadr v*) 'type) 'number) )
        (k (create-number (* ((car v*) 'value)
                             ((cadr v*) 'value) ))
           s )
        (wrong "* require numbers") ) )
  2 )

(defprimitive remainder
  (lambda (v* s k)
    (if (and (eq? ((car v*) 'type) 'number)
             (eq? ((cadr v*) 'type) 'number) )
        (k (create-number 
            (remainder ((car v*) 'value) ((cadr v*) 'value)) ) s)
        (wrong "remainder require numbers") ) )
  2 )

(defprimitive cons
  (lambda (v* s k)
    (allocate-pair (car v*) (cadr v*) s k) )
  2 )

(defprimitive car
  (lambda (v* s k)
    (if (eq? ((car v*) 'type) 'pair)
        (k (s ((car v*) 'car)) s) 
        (wrong "Not a pair" (car v*)) ) )
  1 ) 

(defprimitive cdr
  (lambda (v* s k)
    (if (eq? ((car v*) 'type) 'pair)
        (k (s ((car v*) 'cdr)) s) 
        (wrong "Not a pair" (car v*)) ) )
  1 ) 

(defprimitive set-car!
  (lambda (v* s k)
    (if (eq? ((car v*) 'type) 'pair)
        (let ((pair (car v*)))
          (k pair ((pair 'set-car) s (cadr v*))) )
        (wrong "Not a pair" (car v*)) ) )
  2 )

(defprimitive set-cdr!
  (lambda (v* s k)
    (if (eq? ((car v*) 'type) 'pair)
        (let ((pair (car v*)))
          (k pair ((pair 'set-cdr) s (cadr v*))) )
        (wrong "Not a pair" (car v*)) ) )
  2 )

(defprimitive pair?
  (lambda (v* s k)
    (k (create-boolean (eq? ((car v*) 'type) 'pair)) s) )
  1 )

(defprimitive symbol?
  (lambda (v* s k)
    (k (create-boolean (eq? ((car v*) 'type) 'symbol)) s) )
  1 )

(defprimitive eq?
  (lambda (v* s k)
    (k (create-boolean
        (if (eq? ((car v*) 'type) ((cadr v*) 'type))
            (case ((car v*) 'type)
              ((null) #t)
              ((boolean)
               (((car v*) 'boolify)
                (((cadr v*) 'boolify) #t #f)
                (((cadr v*) 'boolify) #f #t) ) )
              ((symbol)
               (eq? ((car v*) 'name) ((cadr v*) 'name)) )
              ((string)
               (eq? ((car v*) 'chars) ((cadr v*) 'chars)) )
              ((pair)
               (and (= ((car v*) 'car) ((cadr v*) 'car))
                    (= ((car v*) 'cdr) ((cadr v*) 'cdr)) ) )
              ((function)
               (= ((car v*) 'tag) ((cadr v*) 'tag)) )
              (else #f) )
            #f ) )
       s ) ) 
  2 ) 

;;; Note: In fact, symbols and strings should be allocated (ie given a
;;; tag representing an address) and compared with their address. This
;;; will require to manage eq-ness of symbols (with an hash-table)
;;; when their pnames are equal.

;;; We define eqv? to be similar to eq? except on numbers.
(defprimitive eqv?
  (lambda (v* s k)
    (k (create-boolean
        (if (eq? ((car v*) 'type) ((cadr v*) 'type))
            (case ((car v*) 'type)
              ((null) #t)
              ((boolean)
               (((car v*) 'boolify)
                (((cadr v*) 'boolify) #t #f)
                (((cadr v*) 'boolify) #f #t) ) )
              ((symbol)
               (eq? ((car v*) 'name) ((cadr v*) 'name)) )
              ((number)
               (= ((car v*) 'value) ((cadr v*) 'value)) )
              ((pair)
               (and (= ((car v*) 'car) ((cadr v*) 'car))
                    (= ((car v*) 'cdr) ((cadr v*) 'cdr)) ) )
              ((function)
               (= ((car v*) 'tag) ((cadr v*) 'tag)) )
              (else #f) )
            #f ) )
       s ) ) 
  2 ) 

(definitial apply
  (create-function
   -11 (lambda (v* s k)
         (define (first-pairs v*)
           ;; (assume (pair? v*))
           (if (pair? (cdr v*))
               (cons (car v*) (first-pairs (cdr v*)))
               '() ) )
         (define (terms-of v s)
           (if (eq? (v 'type) 'pair)
               (cons (s (v 'car)) (terms-of (s (v 'cdr)) s))
               '() ) )
         (if (>= (length v*) 2)
             (if (eq? ((car v*) 'type) 'function)
                 (((car v*) 'behavior) 
                  (append (first-pairs (cdr v*))
                          (terms-of (car (last-pair (cdr v*))) s) )
                  s k )
                 (wrong "First argument not a function") )
             (wrong "Incorrect arity for apply") ) ) ) )

(definitial list
  (create-function -12 allocate-list) )

(definitial call/cc
  (create-function
   -13 (lambda (v* s k)
         (if (= 1 (length v*))
             (if (eq? ((car v*) 'type) 'function)
                 (allocate 1 s
                  (lambda (a* ss)
                    (((car v*) 'behavior) 
                     (list (create-function
                            (car a*)
                            (lambda (vv* sss kk)
                              (if (= 1 (length vv*))
                                  (k (car vv*) sss)
                                  (wrong "Incorrect arity") ) ) ) )
                     ss k ) ) )
                 (wrong "Non functional argument for call/cc") )
             (wrong "Incorrect arity for call/cc") ) ) ) )


;;; (file-test "src/chap4a.tst")
;;; (file-test "../scheme-tests.scm")
(define (file-test file)
  (suite-test file
              "?? " "== " #t
              (lambda (read check err)
                (set! wrong err)
                (let ((s.current s.global))
                  (lambda ()
                    ;; reset memorized quotations.
                    (set! *shared-memo-quotations* '())
                    (check (evaluate (read)
                                     r.global
                                     s.current
                                     (lambda (v ss)
                                       (set! s.current ss)
                                       (transcode-back v ss) ) )) ) ) )
              naive-match ) )

;;; Define some locations to test the interpreter

(definitial a 0)
(definitial foo 0)
(definitial fact 0)
(definitial fib 0)
(definitial primes 0)

;;; end of chap4a.scm
