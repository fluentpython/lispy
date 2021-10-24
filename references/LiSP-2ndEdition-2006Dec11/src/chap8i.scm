;;; $Id: chap8i.scm,v 4.2 2006/11/24 18:40:39 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Add the import special form 

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
        ((import)  (meaning-import (cadr e) (caddr e) (cdddr e) r tail?)) ; \modified
        (else      (meaning-application (car e) (cdr e) r tail?)) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; (import (variables...) env forms...) a new special form that evaluates
;;; forms with variables taken out of env.

(define (meaning-import n* e e+ r tail?)
  (let* ((m (meaning e r #f))
         (r2 (shadow-extend* r n*))
         (m+ (meaning-sequence e+ r2 #f)) )
    (append (CONSTANT n*) (PUSH-VALUE) m (CREATE-PSEUDO-ENV) 
            (if tail? m+ (append m+ (UNLINK-ENV))) ) ) )
      

(define (CREATE-PSEUDO-ENV)   (list 252))
(define (SHADOWABLE-REF i j)  (list 231 i j))
(define (SHADOWABLE-SET! i j m) 
  (append m (list 232 i j)) )

(define-class pseudo-activation-frame environment
  ( sr (* address) ) )

(define-method (show (a pseudo-activation-frame) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "[Pseudo env addresses" stream)
    (do ((i 0 (+ 1 i)))
        ((>= i (pseudo-activation-frame-address-length a)))
      (display (list i '=>) stream)
      (show (pseudo-activation-frame-address a i) stream) )
    (display ", sr=" stream)
    (show (pseudo-activation-frame-sr a) stream)
    (display ", next=" stream)
    (show (environment-next a) stream)
    (display "]" stream) ) )

(define (shadow-extend* r n*)
  (let enum ((n* n*)(j 0))
    (if (pair? n*)
        (cons (list (car n*) `(shadowable 0 . ,j))
              (enum (cdr n*) (+ j 1)) )
        (bury-r r 1) ) ) )

(define (bury-r r offset)
  (map (lambda (d)
         (let ((name (car d))
               (type (car (cadr d))) )
           (case type
             ((local checked-local shadowable) 
              (let* ((addr (cadr d))
                     (i (cadr addr))
                     (j (cddr addr)) )
                `(,name (,type ,(+ i offset) . ,j) . ,(cddr d)) ) )
             (else d) ) ) )
       r ) )

;;; Retrofit to add shadowable

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
          ((shadowable)
           (let ((i (cadr kind))
                 (j (cddr kind)) )
             (SHADOWABLE-REF i j) ) )
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
          ((shadowable)
           (let ((i (cadr kind))
                 (j (cddr kind)) )
             (SHADOWABLE-SET! i j m) ) )
          ((global)
           (let ((i (cdr kind)))
             (GLOBAL-SET! i m) ) )
          ((predefined)
           (static-wrong "Immutable predefined variable" n) ) )
        (static-wrong "No such variable" n) ) ) )

(define (create-pseudo-environment n* env sr)
  (unless (reified-environment? env)
    (signal-exception #f (list "not an environment" env)) )
  (let* ((len (length n*))
         (frame (allocate-pseudo-activation-frame len)) )
    (let setup ((n* n*)(i 0))
      (when (pair? n*)
        (set-pseudo-activation-frame-address!
         frame i (compute-kind (reified-environment-r env) (car n*)) )
        (setup (cdr n*) (+ i 1)) ) )
    (set-pseudo-activation-frame-sr! frame 
                                     (reified-environment-sr env) )
    (set-pseudo-activation-frame-next! frame sr)
    (set! *env* frame) ) )

(define (shadowable-fetch sr i j)
  (if (= i 0)
      (let ((kind (pseudo-activation-frame-address sr j))
            (sr (pseudo-activation-frame-sr sr)) )
        (variable-value-lookup kind sr) )
      (shadowable-fetch (environment-next sr) (- i 1) j) ) )

(define (shadowable-update! sr i j value)
  (if (= i 0)
      (let ((kind (pseudo-activation-frame-address sr j))
            (sr (pseudo-activation-frame-sr sr)) )
        (variable-value-update! kind sr value) )
      (shadowable-update! (environment-next sr) (- i 1 ) j value) ) )

;;; end of chap8i.scm
