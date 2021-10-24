;;; $Id: chap6c.scm,v 4.1 2006/11/13 12:16:23 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;;                      Threaded interpreter.
;;;  Environment is held by a global variable. This is bad for //ism.

;;; Load chap6a.scm before and redefine meaning and related functions.

(define *env* sr.init)

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooNEW
;;; The threaded interpreter.
;;; E is the expression to evaluate
;;; R is the representation of the local lexical environment
;;; TAIL? is a boolean that indicates if E is a terminal call (also
;;; means whether the *env* register should be restored or not).

(define (meaning e r tail?)
  (if (atom? e)
      (if (symbol? e) (meaning-reference e r tail?)
                      (meaning-quotation e r tail?) )
      (case (car e)
        ((quote)  (meaning-quotation (cadr e) r tail?))
        ((lambda) (meaning-abstraction (cadr e) (cddr e) r tail?))
        ((if)     (meaning-alternative (cadr e) (caddr e) (cadddr e) 
                                       r tail? ))
        ((begin)  (meaning-sequence (cdr e) r tail?))
        ((set!)   (meaning-assignment (cadr e) (caddr e) r tail?))
        (else     
         (meaning-application (car e) (cdr e) r tail?) ) ) ) )

(define (meaning-reference n r tail?)
  (let ((kind (compute-kind r n)))
    (if kind
        (case (car kind)
          ((local)
           (let ((i (cadr kind))
                 (j (cddr kind)) )
             (if (= i 0)
                 (lambda (k)
                   (k (activation-frame-argument *env* j)) )
                 (lambda (k)
                   (k (deep-fetch *env* i j)) ) ) ) )
          ((global)
           (let ((i (cdr kind)))
             (if (eq? (global-fetch i) undefined-value)
                 (lambda (k)
                   (let ((v (global-fetch i)))
                     (if (eq? v undefined-value)
                         (wrong "Uninitialized variable" n)
                         (k v)  ) ) )
                 (lambda (k)
                   (k (global-fetch i)) ) ) ) )
          ((predefined)
           (let* ((i (cdr kind))
                  (value (predefined-fetch i)) )
             (lambda (k)
               (k value) ) ) ) )
        (static-wrong "No such variable" n) ) ) )

(define (meaning-quotation v r tail?)
  (lambda (k)
    (k v) ) )

(define (meaning-alternative e1 e2 e3 r tail?)
  (let ((m1 (meaning e1 r #f))         ; restore environment!
        (m2 (meaning e2 r tail?))
        (m3 (meaning e3 r tail?)) )
    (lambda (k)
      (m1 (lambda (v)
            ((if v m2 m3) k) )) ) ) )

(define (meaning-assignment n e r tail?) 
  (let ((m (meaning e r #f))
        (kind (compute-kind r n)) )
    (if kind
        (case (car kind)
          ((local)
           (let ((i (cadr kind))
                 (j (cddr kind)) )
             (if (= i 0)
                 (lambda (k)
                   (m (lambda (v)
                        (k (set-activation-frame-argument! *env* j v)) )) )
                 (lambda (k)
                   (m (lambda (v)
                        (k (deep-update! *env* i j v)) )) ) ) ) )
          ((global)
           (let ((i (cdr kind)))
             (lambda (k)
               (m (lambda (v)
                    (k (global-update! i v)) )) ) ) )
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
    (lambda (k)
      (m1 (lambda (v)
            (m+ k) )) ) ) )

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
         (arity+1 (+ arity 1))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence e+ r2 #t)) )
    (lambda (k)
      (let ((sr *env*))
        (k (lambda (v* k1)
             (if (= (activation-frame-argument-length v*) arity+1)
                 (begin (set! *env* (sr-extend* sr v*))
                        (m+ k1) )
                 (wrong "Incorrect arity") ) )) ) ) ) )

(define (meaning-dotted-abstraction n* n e+ r tail?)
  (let* ((arity (length n*))
         (arity+1 (+ arity 1))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence e+ r2 #t)) )
    (lambda (k)
      (let ((sr *env*))
        (k (lambda (v* k1)
             (if (>= (activation-frame-argument-length v*) arity+1)
                 (begin (listify! v* arity)
                        (set! *env* (sr-extend* sr v*))
                        (m+ k1) )
                 (wrong "Incorrect arity") ) )) ) ) ) )

;;; Application meaning.

(define (meaning-application e e* r tail?)
  (cond ((and (symbol? e)
              (let ((kind (compute-kind r e)))
                (and (pair? kind)
                     (eq? 'predefined (car kind))
                     (let ((desc (get-description e)))
                       (and desc
                            (eq? 'function (car desc))
                            (= (length (cddr desc)) (length e*)) ) ) ) ) )
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
    (if tail?
        (lambda (k)
          (m* (lambda (v*)
                (set! *env* (sr-extend* *env* v*))
                (m+ k) )) )
        (lambda (k)
          (let* ((sr *env*)
                 (k (lambda (v) (set! *env* sr) (k v))) )
            (m* (lambda (v*)
                  (set! *env* (sr-extend* *env* v*))
                  (m+ k) )) ) ) ) ) )

(define (meaning-dotted-closed-application n* n body e* r tail?)
  (let* ((m* (meaning-dotted* e* r (length e*) (length n*) #f))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence body r2 tail?)) )
    (if tail?
        (lambda (k)
          (m* (lambda (v*)
                (set! *env* (sr-extend* *env* v*))
                (m+ k) )) )
        (lambda (k)
          (let* ((sr *env*)
                 (k (lambda (v) (set! *env* sr) (k v))) )
            (m* (lambda (v*)
                  (set! *env* (sr-extend* *env* v*))
                  (m+ k) )) ) ) ) ) )

;;; Handles a call to a predefined primitive. The arity is already checked.
;;; The optimization is to avoid the allocation of the activation frame.
;;; These primitives never change the *env* register nor have control effect.

(define (meaning-primitive-application e e* r tail?)
  (let* ((desc (get-description e))
         ;; desc = (function <address> . <variables-list>)
         (address (cadr desc))
         (size (length e*)) )
    (case size
      ((0) (lambda (k) (k (address))))
      ((1) 
       (let ((m1 (meaning (car e*) r tail?)))
         (lambda (k) 
           (m1 (lambda (v) 
                 (k (address v)) )) ) ) )
      ((2) 
       (let ((m1 (meaning (car e*) r #f))
             (m2 (meaning (cadr e*) r tail?)) )
         (lambda (k) 
           (m1 (lambda (v1)
                 (m2 (lambda (v2)
                       (k (address v1 v2)) )) )) ) ) )
      ((3) 
       (let ((m1 (meaning (car e*) r #f))
             (m2 (meaning (cadr e*) r #f))
             (m3 (meaning (caddr e*) r tail?)) )
         (lambda (k) 
           (m1 (lambda (v1)
                 (m2 (lambda (v2)
                       (m3 (lambda (v3)
                             (k (address v1 v2 v3)) )) )) )) ) ) )
      (else (meaning-regular-application e e* r tail?)) ) ) )

;;; In a regular application, the invocation protocol is to call the
;;; function with an activation frame and a continuation: (f v* k).

(define (meaning-regular-application e e* r tail?)
  (let* ((m (meaning e r #f))
         (m* (meaning* e* r (length e*) #f)) )
    (if tail?
        (lambda (k)
          (m (lambda (f)
               (if (procedure? f)
                   (m* (lambda (v*)
                         (f v* k) ))
                   (wrong "Not a function" f) ) )) )
        (lambda (k)
          (m (lambda (f)
               (if (procedure? f)
                   (m* (lambda (v*)
                         (let ((sr *env*))         ; save environment
                           (f v* (lambda (v) 
                                   (set! *env* sr) ; restore environment
                                   (k v) )) ) ))
                   (wrong "Not a function" f) ) )) ) ) ) )

(define (meaning* e* r size tail?)
  (if (pair? e*)
      (meaning-some-arguments (car e*) (cdr e*) r size tail?)
      (meaning-no-argument r size tail?) ) )

(define (meaning-dotted* e* r size arity tail?)
  (if (pair? e*)
      (meaning-some-dotted-arguments (car e*) (cdr e*) r size arity tail?)
      (meaning-no-dotted-argument r size arity tail?) ) )

(define (meaning-some-arguments e e* r size tail?)
  (let ((m (meaning e r #f))
        (m* (meaning* e* r size tail?))
        (rank (- size (+ (length e*) 1))) )
    (lambda (k)
      (m (lambda (v)
           (m* (lambda (v*)
                 (set-activation-frame-argument! v* rank v)
                 (k v*) ) )) )) ) )

(define (meaning-some-dotted-arguments e e* r size arity tail?)
  (let ((m (meaning e r #f))
        (m* (meaning-dotted* e* r size arity tail?))
        (rank (- size (+ (length e*) 1))) )
    (if (< rank arity)
        (lambda (k)
          (m (lambda (v)
               (m* (lambda (v*)
                     (set-activation-frame-argument! v* rank v)
                     (k v*) )) )) )
        (lambda (k)
          (m (lambda (v)
               (m* (lambda (v*)
                     (set-activation-frame-argument! 
                      v* arity
                      (cons v (activation-frame-argument v* arity)) )
                     (k v*) )) )) ) ) ) )

(define (meaning-no-argument r size tail?)
  (let ((size+1 (+ size 1)))
    (lambda (k)
      (let ((v* (allocate-activation-frame size+1)))
        (k v*) ) ) ) )

(define (meaning-no-dotted-argument r size arity tail?)
  (let ((arity+1 (+ arity 1)))
    (lambda (k)
      (let ((v* (allocate-activation-frame arity+1)))
        (set-activation-frame-argument! v* arity '())
        (k v*) ) ) ) )

;;; To suppress optimization on closed application or inlining
;;; invocations to primitives.

;(set! meaning-application meaning-regular-application)   ;; TEMP
;(set! meaning-closed-application meaning-regular-application)   ;; TEMP

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Initialization of the predefined global environment.

;;; We do not need to save the register *env* since call/cc is not a
;;; primitive (it is defined by definitial and not by defprimitive)
;;; and non-primitive invokations are regularly handled.

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Testing

(define (chapter62-interpreter)
  (define (toplevel)
    (set! *env* sr.init)
    ((meaning (read) r.init #t) display)
    (toplevel) )
  (toplevel) )

(define (scheme6c)
  (interpreter 
   "Scheme? "  
   "Scheme= " 
   #t
   (lambda (read print error)
     (set! wrong error)
     (set! static-wrong error)
     (lambda ()
       (set! *env* sr.init)
       ((meaning (read) r.init #t)
        print ) ) ) ) )

(define (test-scheme6c file)
  (suite-test 
   file 
   "Scheme? " 
   "Scheme= "
   #t
   (lambda (read check error)
     (set! wrong error)
     (set! static-wrong error)
     (lambda ()
       (set! *env* sr.init)
       ((meaning (read) r.init #t)
        check ) ) )
   equal? ) )

;;; Pay attention to tail-rec in Scheme->C.

(define (bench6c factor e)
  (let ((start (get-internal-run-time))
        (m (meaning e r.init #t)) )
    (let loop ((factor factor))
      (set! *env* sr.init)
      (m (lambda (v) 
           (let ((duration (- (get-internal-run-time) start)))
             (when (<= factor 1)
               (display (list duration v))
               (newline) ) ) ) )
      (if (> factor 1)
          (loop (- factor 1)) ) ) ) )

;;; end of chap6c.scm
