;;; $Id: chap8a.scm,v 4.3 2006/11/24 18:41:17 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; Programs of chapter 8 evaluation and reflection

;;; Modification of chap1.scm, introducing eval as a special form.

(define (evaluate e env)
  (if (atom? e) 
      (cond ((symbol? e) (lookup e env))
            ((or (number? e)(string? e)(char? e)(boolean? e)) e)
            (else (wrong "Cannot evaluate" e)) )
      (case (car e)
        ((quote)  (cadr e))
        ((if)     (if (not (eq? (evaluate (cadr e) env) 
                                the-false-value ))
                      (evaluate (caddr e) env)
                      (evaluate (cadddr e) env) ))
        ((begin)  (eprogn (cdr e) env))
        ((set!)   (update! (cadr e) env (evaluate (caddr e) env)))
        ((lambda) (make-function (cadr e) (cddr e) env))
        ((eval)   (evaluate (evaluate (cadr e) env) env)) ; \modified
        (else     (invoke (evaluate (car e) env)
                          (evlis (cdr e) env) )) ) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Recognize programs

(define (program? e)
  (define (program? e m)
    (if (atom? e)
        (or (symbol? e) (number? e) (string? e) 
            (char? e) (boolean? e) )
        (if (memq e m) #f
            (let ((m (cons e m)))
              (define (all-programs? e+ m+)
                (if (memq e+ m+) #f
                    (let ((m+ (cons e+ m+)))
                      (and (pair? e+)
                           (program? (car e+) m)
                           (or (null? (cdr e+))
                               (all-programs? (cdr e+) m+) ) ) ) ) )
              (case (car e)
                ((quote) (and (pair? (cdr e))
                              (quotation? (cadr e))
                              (null? (cddr e)) ))
                ((if) (and (pair? (cdr e))
                           (program? (cadr e) m)
                           (pair? (cddr e))
                           (program? (caddr e) m)
                           (pair? (cdddr e))
                           (program? (cadddr e) m)
                           (null? (cddddr e)) ))
                ((begin) (all-programs? (cdr e) '()))
                ((set!) (and (pair? (cdr e))
                             (symbol? (cadr e))
                             (pair? (cddr e))
                             (program? (caddr e) m)
                             (null? (cdddr e)) ))
                ((lambda) (and (pair? (cdr e))
                               (variables-list? (cadr e))
                               (all-programs? (cddr e) '()) ))
                (else (all-programs? e '())) ) ) ) ) )
  (program? e '()) )

(define (variables-list? v*)
  (define (variables-list? v* already-seen)
    (or (null? v*)
        (and (symbol? v*) (not (memq v* already-seen)))
        (and (pair? v*) 
             (symbol? (car v*))
             (not (memq (car v*) already-seen))
             (variables-list? (cdr v*) 
                              (cons (car v*) already-seen) ) ) ) )
  (variables-list? v* '()) )

(define (quotation? e)
  (define (quotation? e m)
    (if (memq e m) #f
        (let ((m (cons e m)))
          (or (null? e)(symbol? e)(number? e)
              (string? e)(char? e)(boolean? e)
              (and (vector? e)
                   (let loop ((i 0))
                     (or (>= i (vector-length e))
                         (and (quotation? (vector-ref e i) m)
                              (loop (+ i 1)) ) ) ) )
              (and (pair? e)
                   (quotation? (car e) m)
                   (quotation? (cdr e) m) ) ) ) ) )
  (quotation? e '()) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; These are the new functions to access the environment. They use
;;; the reflective global-value and set-global-value! functions.

(define (lookup id env)
  (if (pair? env)
      (if (eq? (caar env) id)
          (cdar env)
          (lookup id (cdr env)) )
      (global-value id) ) )

(define (update! id env value)
  (if (pair? env)
      (if (eq? (caar env) id)
          (begin (set-cdr! (car env) value)
                 value )
          (update! id (cdr env) value) )
      (set-global-value! id value) ) ) 

;;; A quick and dirty simulation of global-value and set-global-value!

(define (global-value id)
  (wrong "No such global variable" id) )

(define (fixed-set-global-value! id val)
  (wrong "No such global variable" id) )

;;; This variant allows creation of dynamic variables.
(define (dynamic-set-global-value! id val)
  (append! env.global (list (cons id val)))
  val )

(define set-global-value! fixed-set-global-value!)

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Testing program? and quotation?

(define (test-program file)
  (suite-test
   file
   "Scheme? "
   "Scheme= "
   #t
   (lambda (read check error)
     (set! wrong error)
     (lambda ()
       (check (eval (read))) ) )
   equal? ) )

;;; end of chap8a.scm
