;;; $Id: chap5g.scm,v 4.0 1995/07/10 06:51:31 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; With global environment.

;   Meaning  : Form -> Env * GlobalEnv * Cont * Store -> Val
; r   Env    : Var -> Loc
; g   GlobalEnv    : Var -> Loc
; s   Store  : Loc -> Val
; k   Cont   : Val * GlobalEnv * Store -> Val
; v   Val    : Fun | Int | Pair
; f   Fun    : (Val...) * GlobalEnv * Cont * Store -> Val
;     Cons   : Loc * Loc
; e   Form

;;; Injections  and projections
(define-class Value Object (content))

(define (inValue f)
  (make-Value f) )

(define (Value->Function e)
  (let ((c (Value-content e)))
    (if (procedure? c) c
        (wrong "Not a function" c) ) ) )

(define (Value->Pair e)
  (let ((c (Value-content e)))
    (if (cons? c) c
        (wrong "Not a pair" c) ) ) )

(define (Value->Integer e)
  (let ((c (Value-content e)))
    (if (integer? c) c
        (wrong "Not a pair" c) ) ) )

;;; Syntactical analysis

(define (meaning e)
  (if (atom? e)
      (if (symbol? e) (meaning-reference e)
                      (meaning-quotation e) )
      (case (car e)
        ((quote)  (meaning-quotation (cadr e)))
        ((lambda) (meaning-abstraction (cadr e) (cddr e)))
        ((if)     (meaning-alternative (cadr e) (caddr e) (cadddr e)))
        ((begin)  (meaning-sequence (cdr e)))
        ((set!)   (meaning-assignment (cadr e) (caddr e)))
        ((define) (meaning-definition (cadr e) (caddr e)))
        ((global) (meaning-global-reference (cadr e)))
        (else     (meaning-application (car e) (cdr e))) ) ) )

(define ((meaning-quotation v) r g k s)
  (translate v s (lambda (v s) (k v g s))) )

(define ((meaning-global-reference n) r g k s)
  (let ((a (g n)))
    (if (eq? a no-such-global-binding)
        (wrong "No such variable" n)
        (k (s a) g s) ) ) )
  
(define (meaning-reference n) 
  (lambda (r g k s)
    (let ((a (r n)))
      (if (eq? a no-such-binding)
          (let ((a (g n)))
            (if (eq? a no-such-global-binding)
                (wrong "No such variable" n)
                (k (s a) g s) ) )
          (k (s a) g s) ) ) ) )

;;; Extensional alternative

(define (ef v v1 v2)
  (v v1 v2) )

(define (boolify v)
  (if (equal? v (inValue #f))
      (lambda (x y) y) 
      (lambda (x y) x) ) )

(define ((meaning-alternative e1 e2 e3) r g k s)
  ((meaning e1) r
                g 
                (lambda (v g1 s1)
                  (ef (boolify v)
                      ((meaning e2) r g1 k s1) 
                      ((meaning e3) r g1 k s1) ) )
                s ) )

(define ((meaning-other-alternative e1 e2 e3) r g k s)
  ((meaning e1) r
                g 
                (lambda (v g1 s1)
                  ((ef (boolify v) (meaning e2) (meaning e3))
                   r g1 k s1 ) )
                s ) )

(define ((meaning-assignment n e) r g k s)
  ((meaning e) r
               g 
               (lambda (v g1 s1)
                 (let ((a (r n)))
                   (if (eq? a no-such-binding)
                       (let ((a (g1 n)))
                         (if (eq? a no-such-global-binding)
                             (wrong "No such variable" n)
                             (k v g1 (extend s1 a v)) ) )
                       (k v g1 (extend s1 a v)) ) ) )
               s ) )

(define ((meaning-definition n e) r g k s)
  ((meaning e) r
               g 
               (lambda (v g1 s1)
                 (let ((a (g n)))
                   (if (eq? a no-such-global-binding)
                       (allocate s1 1
                                 (lambda (s2 a*)
                                   (k v 
                                      (extend g1 n (car a*))
                                      (extend s2 (car a*) v) ) ) )
                       (k v g1 (extend s1 a v)) ) ) )
               s ) )

(define ((meaning-global-definition n e) r g k s)
  ((meaning e) r
               g 
               (lambda (v g1 s1)
                 (let ((a (g n)))
                   (if (eq? a no-such-global-binding)
                       (allocate s1 1
                                 (lambda (s2 a*)
                                   (k v 
                                      (extend g1 n (car a*))
                                      (extend s2 (car a*) v) ) ) )
                       (k v g1 (extend s1 a v)) ) ) )
               s ) )

(define ((meaning-defining-assignment n e) r g k s)
    ((meaning e) r
               g 
               (lambda (v g1 s1)
                 (let ((a (r n)))
                   (if (eq? a no-such-binding)
                       (let ((a (g1 n)))
                         (if (eq? a no-such-global-binding)
                             (allocate s1 1
                                 (lambda (s2 a*)
                                   (k v 
                                      (extend g1 n (car a*))
                                      (extend s2 (car a*) v) ) ) )
                             (k v g1 (extend s1 a v)) ) )
                       (k v g1 (extend s1 a v)) ) ) )
               s ) )

;;; This denotation does not allow dotted variables.

(define ((meaning-abstraction n* e+) r g k s)
  (k (inValue (lambda (v* g1 k1 s1)
                (if (= (length v*) (length n*))
                    (allocate s1 (length n*)
                              (lambda (s2 a*)
                                ((meaning*-sequence e+) 
                                 (extend* r n* a*)
                                 g1 
                                 k1
                                 (extend* s2 a* v*) ) ) )
                    (wrong "Incorrect arity") ) ))
     g 
     s ) )

(define ((meaning-hyperstatic-abstraction n* e+) r g k s)
  (k (inValue (lambda (v* g2 k1 s1)
                (if (= (length v*) (length n*))
                    (allocate s1 (length n*)
                              (lambda (s2 a*)
                                ((meaning*-sequence e+) 
                                 (extend* r n* a*)
                                 g
                                 k1
                                 (extend* s2 a* v*) ) ) )
                    (wrong "Incorrect arity") ) ))
     g 
     s ) )

;;; Nor does this one. Meaning-fix-abstraction is the same as the
;;; previous one except that it is written in a form that makes it
;;; similar to meaning-dotted-abstraction.

(define ((meaning-fix-abstraction n* e+) r g k s)
  (k (inValue (lambda (v* g1 k1 s1)
                (if (= (length v*) (length n*))
                    (((meaning-regular-variables n*)
                     (lambda (v* r g k s) ((meaning*-sequence e+) r g k s)) )
                     v* r g1 k1 s1 )
                    (wrong "Incorrect arity") ) ))
     g s ) )

(define (meaning-regular-variables n*)
  (if (pair? n*) 
      (meaning-some-regular-variables (car n*) (cdr n*))
      (meaning-no-regular-variables) ) )

(define ((meaning-no-regular-variables) m)
  m )

(define ((meaning-some-regular-variables n n*) m)
  ((meaning-variable n) ((meaning-regular-variables n*) m)) )

(define ((meaning-variable n) m) 
  (lambda (v* r g k s)
    (allocate 
     s 1 (lambda (s a*)
           (let ((a (car a*)))
             (m (cdr v*) (extend r n a) g k (extend s a (car v*))) ) ) ) ) )

(define (meaning-possibly-dotted-abstraction n* e+)
  (let parse ((n* n*)
              (regular '()) )
    (cond 
     ((pair? n*) (parse (cdr n*) (cons (car n*) regular)))
     ((null? n*) (meaning-fix-abstraction (reverse regular) e+))
     (else       (meaning-dotted-abstraction (reverse regular) n* e+)) ) ) )

(define ((meaning-dotted-abstraction n* n e+) r g k s)
  (k (inValue (lambda (v* g1 k1 s1)
                (if (>= (length v*) (length n*))
                    (((meaning-regular-variables n*)
                      ((meaning-dotted-variable n) 
                       (lambda (v* r g k s) 
                         ((meaning*-sequence e+) r g k s) ) ) )
                     v* r g1 k1 s1 )
                    (wrong "Incorrect arity") ) ))
     g
     s ) )

(define ((meaning-dotted-variable n) m)
  (lambda (v* r g k s)
    (letrec ((listify 
              (lambda (v* g s q)
                (if (pair? v*)
                    (allocate 
                     s 2 (lambda (s a*)
                           (let ((qq (lambda (v g s)
                                       (q (inValue a*)
                                          g
                                          (extend s (cadr a*) v) ) )))
                             (listify (cdr v*)
                                      g
                                      (extend s (car a*) (car v*))
                                      qq ) ) ) )
                    (q (inValue (list)) g s) ) )))
      (listify v* g s (lambda (v g s)
                        (allocate s 1
                                  (lambda (s a*)
                                    (let ((a (car a*)))
                                      (m (list) 
                                         (extend r n a)
                                         g 
                                         k 
                                         (extend s a v) ) ) ) ) )) ) ) )

;;; Retrofit!

(set! meaning-abstraction meaning-possibly-dotted-abstraction)
(set! meaning-alternative meaning-other-alternative)


(define ((meaning-application e e*) r g k s)
  ((meaning e) r
               g 
               (lambda (f g1 s1)
                 ((meaning* e*) r
                                g1
                                (lambda (v* g2 s2)
                                  ((Value->Function f) v* g2 k s2) )
                                s1 ) )
               s ) )

(define ((meaning-exercice-application e e*) r g k s)
  ((meaning e) r 
               g
               (lambda (f g1 ss)
                 ((meaning-stack-application e*)
                  (list)
                  r 
                  g 
                  (lambda (v* g s) ((Value->Function f) v* g k s))
                  ss ) )
               s ) )

(define (meaning-stack-application e*)
  (if (pair? e*) 
      (meaning-stack-some-arguments (car e*) (cdr e*))
      (meaning-stack-no-arguments) ) )

(define ((meaning-stack-no-arguments) v* r g k s)
  (k (reverse v*) g s) )

(define ((meaning-stack-some-arguments e e*) v* r g k s)
  ((meaning e) r 
               g
               (lambda (v g ss)
                 ((meaning-stack-application e*)
                  (cons v v*) r g k ss ) )
               s ) )
;;; tests
(set! meaning-application meaning-exercice-application)

;;; iterator aka eprogn

(define ((meaning-sequence e+) r g k s)
  ((meaning*-sequence e+) r g k s) )

(define (meaning*-sequence e+)
  (if (pair? e+)
      (if (pair? (cdr e+))
          (meaning*-multiple-sequence (car e+) (cdr e+))
          (meaning*-single-sequence (car e+)) )
      (wrong "Illegal syntax") ) )

;;; The meaning-single-sequence function can be eta-simplified as well as
;;; the meaning-sequence function above.

(define ((meaning*-single-sequence e) r g k s)
  ((meaning e) r g k s) )

(define ((meaning*-multiple-sequence e e+) r g k s)
  ((meaning e) r
               g 
               (lambda (v g1 s1)
                 ((meaning*-sequence e+) r g1 k s1) )
               s ) )

;;; Iterator aka evlis

(define (meaning* e*)
  (if (pair? e*)
      (meaning-some-arguments (car e*) (cdr e*))
      (meaning-no-argument) ) )

(define ((meaning-some-arguments e e*) r g k s)
  ((meaning e) r
               g
               (lambda (v g1 s1)
                 ((meaning* e*) r 
                                g
                                (lambda (v* g2 s2)
                                  (k (cons v v*) g2 s2) )
                                s1 ) )
               s ) )

(define ((meaning-no-argument) r g k s)
  (k (list) g s) )

;;; Representation of environment and store

(define no-such-binding (list 'no-such-binding))
(define no-such-global-binding (list 'no-such-global-binding))

(define (s.init a)
  (wrong "no such address" a) )

(define (g.init n)
  no-such-global-binding )

(define (r.init n)
  no-such-binding )

(define (extend fn pt im)
  (lambda (x) (if (equal? pt x) im (fn x))) )

(define (extend* fn pts ims)
  (if (pair? pts)
      (extend (extend* fn (cdr pts) (cdr ims))
              (car pts) (car ims) )
      fn ) )

;;; Locations are represented by growing positive integers.
;;; No attempt is done to garbage collect locations.
;;; Moreover allocate is not a real function since it allocates new
;;; unseen locations each time it is invoked.

(define allocate 
  (let ((loc 0))
    (lambda (s n q)
      (let loop ((n n)
                 (a* '()) )
        (if (<= n 0) (q s a*)
                     (begin (set! loc (- loc 1))
                            (loop (- n 1)
                                  (cons loc a*) ) ) ) ) ) ) )

;;; Two macros to define the original environment and store.

(define-syntax definitial 
  (syntax-rules ()
    ((definitial name value)
     (allocate s.init 1
               (lambda (s a*)
                 (set! g.init (extend g.init 'name (car a*)))
                 (set! s.init (extend s (car a*) value))
                 'name ) ) ) ) )

(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial name 
       (inValue 
        (lambda (v* g k s) 
          (if (= arity (length v*))
              (k (apply value v*) g s)
              (wrong "Incorrect arity" 'name) ) ) ) ) ) ) )

(define-syntax defarithmetic
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial name 
       (inValue 
        (lambda (v* g k s) 
          (if (= arity (length v*))
              (k (inValue (apply value (map Value->Integer v*))) g s)
              (wrong "Incorrect arity" 'name) ) ) ) ) ) ) )

(definitial cons 
  (inValue 
   (lambda (v* g k s)
     (if (= 2 (length v*))
         (allocate s 2
                   (lambda (s a*) (k (inValue a*) g (extend* s a* v*))) )
         (wrong "incorrect arity" 'cons) ) ) ) )

(definitial car 
  (inValue 
   (lambda (v* g k s)
     (if (= 1 (length v*))
         (k (s (car (Value->Pair (car v*)))) g s)
         (wrong "incorrect arity" 'car) ) ) ) )

(definitial cdr 
  (inValue 
   (lambda (v* g k s)
     (if (= 1 (length v*))
         (k (s (cadr (Value->Pair (car v*)))) g s)
         (wrong "incorrect arity" 'cdr) ) ) ) )

(defprimitive pair? 
  (lambda (v) (inValue (cons? (Value-content v))))
  1 )

;;; Hack
(defprimitive eq? 
  (lambda (v1 v2) (inValue (eq? (Value-content v1)
                                (Value-content v2) )))
  2 )

(defprimitive symbol? 
  (lambda (v) (inValue (symbol? (Value-content v))))
  1 )

(definitial set-car! 
  (inValue
   (lambda (v* g k s)
     (if (= 2 (length v*))
         (k (car v*)
            g
            (extend s (car (Value->Pair (car v*)))
                    (cadr v*) ) ) 
         (wrong "incorrect arity" 'set-car!) ) ) ) )

(definitial set-cdr! 
  (inValue
   (lambda (v* g k s)
     (if (= 2 (length v*))
         (k (car v*)
            g
            (extend s (cadr (Value->Pair (car v*)))
                    (cadr v*) ) ) 
         (wrong "incorrect arity" 'set-cdr!) ) ) ) )

(defarithmetic + + 2)
(defarithmetic - - 2)
(defarithmetic < < 2)
(defarithmetic > > 2)
(defarithmetic = = 2)
(defarithmetic * * 2)
(defarithmetic <= <= 2)
(defarithmetic >= >= 2)
(defarithmetic remainder remainder 2)

(definitial call/cc
  (inValue
   (lambda (v1* g1 k1 s1) 
     (if (= 1 (length v1*))
         ((Value->Function (car v1*))
          (list (inValue 
                 (lambda (v2* g2 k2 s2)
                   (if (= 1 (length v2*))
                       (k1 (car v2*) g2 s2)
                       (wrong "Incorrect arity" 'k) ) ) ))
          g1
          k1
          s1 )
         (wrong "Incorrect arity" 'call/cc) ) ) ) )

(definitial apply
  (inValue
   (lambda (v* g k s)
     (if (>= (length v*) 2)
         (letrec 
             ((collect 
               (lambda (v*)
                 (if (null? (cdr v*))
                     (flat (car v*))
                     (cons (car v*) (collect (cdr v*))) ) ))
              (flat 
               (lambda (v)
                 (if (cons? (Value-content v))
                     (cons (s (car (Value->Pair v)))
                           (flat (s (cadr (Value->Pair v)))) )
                     (list) ) ) ) )
           ((Value->Function (car v*))
            (collect (cdr v*))
            g k s ) )
         (wrong "Incorrect arity" 'apply) ) ) ) )

(definitial list
  (inValue 
   (lambda (v* g k s)
     (let alloc ((v* v*)
                 (s s)
                 (k k) )
       (if (pair? v*)
           (allocate s 2
                     (lambda (s a*)
                       (alloc (cdr v*)
                              (extend s (car a*) (car v*))
                              (lambda (v g s)
                                (k (InValue a*) 
                                   g
                                   (extend s (cadr a*) v) ) ) ) ) )
           (k (InValue '()) g s) ) ) ) ) )

(definitial t (inValue #t))
(definitial f (inValue #f))
(definitial nil (inValue '()))

;;; Some free global locations since they cannot be dynamically defined.

(definitial x 'void)
(definitial y 'void)
(definitial z 'void)
(definitial a 'void)
(definitial b 'void)
(definitial c 'void)
(definitial foo 'void)
(definitial bar 'void)
(definitial hux 'void)
(definitial fib 'void)
(definitial fact 'void)
(definitial visit 'void)
(definitial length 'void)
(definitial primes 'void)

;;; A cons-cell is represented by a sequence of two locations (ie a
;;; list of two integers)

(define (cons? e)
  (and (pair? e)
       (integer? (car e))
       (< (car e) 0)
       (pair? (cdr e))
       (integer? (cadr e))
       (< (cadr e) 0)
       (null? (cddr e)) ) )

(define (convert e s)
  (define (conv e)
    (if (cons? e)
        (cons (conv (Value-content (s (car e))))
              (conv (Value-content (s (cadr e)))) )
        e ) ) 
  (conv (Value-content e)) )

(define (translate e s q)
  (if (pair? e) 
      (translate 
       (car e)
       s
       (lambda (v1 s1)
         (translate 
          (cdr e)
          s1
          (lambda (v2 s2)
            (allocate 
             s2 2 (lambda (s a*)
                    (q (inValue a*)
                       (extend (extend s (car a*) v1)
                               (cadr a*) v2 )) ) ) ) ) ) )
      (q (inValue e) s) ) )

;;; Run the interpreter with:              (denScheme)

(define (denScheme)
  (interpreter 
   "denScheme? "
   "denScheme= "
   #t
   (lambda (read print error)
     (set! wrong error)
     (let ((s.current s.init)
           (g.current g.init) )
       (lambda ()
         ((meaning (read))
          r.init
          g.current
          (lambda (v g.final s.final)
            (set! s.current s.final)
            (set! g.current g.final)
            (print (convert v s.final)) )
          s.current ) ) ) ) ) )

(define (test-denScheme file)
  (suite-test 
   file
   "denScheme? " 
   "denScheme= " 
   #t
   (lambda (read check error)
     (set! wrong error)
     (let ((s.current s.init)
           (g.current g.init) )
       (lambda ()
         ((meaning (read))
          r.init
          g.current
          (lambda (v g.final s.final)
            (set! s.current s.final)
            (set! g.current g.final)
            (check (convert v s.final)) )
          s.current ) ) ) )
   equal? ) )

(define (bench file)
  (let* ((e (call-with-input-file file read))
         (start (get-internal-run-time)) )
    ((meaning e)
     r.init
     g.init
     (lambda (v g s) 
       (let ((duration (- (get-internal-run-time) start)))
         (display (list duration (convert v s))) ) )
     s.init ) ) )     

;;; end of chap5g.scm
