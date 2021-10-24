;;; $Id: chap6dd.scm,v 4.0 1995/07/10 06:51:39 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Variant of chap6d.scm where activation frames are allocated before
;;; evaluation of arguments and where arguments are evaluated from
;;; left to right.

;;; Load before chap6d.scm

(define (FROM-RIGHT-STORE-ARGUMENT m m* rank)
  (lambda ()
    (let* ((v* (m*))
           (v (m)) )
      (set-activation-frame-argument! v* rank v)
      v* ) ) )

(define (FROM-RIGHT-CONS-ARGUMENT m m* arity)
  (lambda ()
    (let* ((v* (m*))
           (v (m)) )
      (set-activation-frame-argument! 
       v* arity (cons v (activation-frame-argument v* arity)) )
      v* ) ) )

;;;  Retrofit

(set! CONS-ARGUMENT FROM-RIGHT-CONS-ARGUMENT)
(set! STORE-ARGUMENT FROM-RIGHT-STORE-ARGUMENT)

;;; Redefine a global variable

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
        ((redefine) (meaning-redefine (cadr e)))
        (else     (meaning-application (car e) (cdr e) r tail?)) ) ) )

(define (meaning-redefine n)
  (let ((kind1 (global-variable? g.init n)))
    (if kind1
        (let ((value (vector-ref sg.init (cdr kind1))))
          (let ((kind2 (global-variable? g.current n)))
            (if kind2
                (static-wrong "Already redefined variable" n)
                (let ((index (g.current-extend! n)))
                  (vector-set! sg.current index value) ) ) ) )
        (static-wrong "No such variable to redefine" n) )
    (lambda () 2001) ) )

;;; For tests, it is necessary not to reset the global modifiable env.
;;; Not very clean but sufficient to test redefine.

(define (stand-alone-producer e)
  ;;(set! g.current (original.g.current))
  (let* ((m (meaning e r.init #t))
         (size (length g.current))
         (names (map (lambda (d) (symbol->string (car d)))
                       (reverse g.current) )) )
    (lambda ()
      ;(set! sg.current (make-vector size undefined-value))
      ;(set! sg.current.names (apply vector names))
      (set! *env* sr.init)
      (m) ) ) )

;;; end of chap6dd.scm
