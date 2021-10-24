;;; $Id: chap9a.scm,v 4.2 2006/11/25 17:45:02 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; Programs of chapter 9 on macros.

;;; Endogeneous macroexpander naive (with only define-abbreviation) 

(define *macros* '())

(define (install-macro! name expander)
  (set! *macros* (cons (cons name expander) *macros*)) )

(define (naive-endogeneous-macroexpander exps)
  (define (macro-definition? exp)
    (and (pair? exp)
         (eq? (car exp) 'define-abbreviation) ) )
  (if (pair? exps)
      (if (macro-definition? (car exps))
          (let* ((def       (car exps))
                 (name      (car (cadr def)))
                 (variables (cdr (cadr def)))
                 (body      (cddr def)) )
            (install-macro! name (macro-eval 
                                  `(lambda ,variables . ,body) ))
            (naive-endogeneous-macroexpander (cdr exps)) )
          (let ((exp (expand-expression (car exps) *macros*)))
            (cons exp (naive-endogeneous-macroexpander 
                       (cdr exps) )) ) )
      '() ) )

(define (expand-expression exp macroenv)
  (define (evaluation? exp)
    (and (pair? exp)
         (eq? (car exp) 'eval-in-abbreviation-world) ) )
  (define (macro-call? exp)
    (and (pair? exp) (find-expander (car exp) macroenv)) )
  (define (expand exp)
    (cond ((evaluation? exp) (macro-eval `(begin . ,(cdr exp))))
          ((macro-call? exp) (expand-macro-call exp macroenv))
          ((pair? exp)
           (let ((newcar (expand (car exp))))
             (cons newcar (expand (cdr exp))) ) )
          (else exp) ) )
  (expand exp) )

(define-abbreviation (define-abbreviation call . body)
  `(eval-in-abbreviation-world
     (install-macro! ',(car call) (lambda ,(cdr call) . ,body))
     #t ) )

(define (simultaneous-eval-macroexpander exps)
  (define (macro-definition? exp)
    (and (pair? exp)
         (eq? (car exp) 'define-abbreviation) ) )
  (if (pair? exps)
      (if (macro-definition? (car exps))
          (let* ((def       (car exps))
                 (name      (car (cadr def)))
                 (variables (cdr (cadr def)))
                 (body      (cddr def)) )
            (install-macro! 
             name (macro-eval `(lambda ,variables . ,body)) )
            (simultaneous-eval-macroexpander (cdr exps)) )
          (let ((e (expand-expression (car exps) *macros*)))
            (eval e)
            (cons e (simultaneous-eval-macroexpander (cdr exps))) ) )
      '() ) )

(define-abbreviation (define-alias newname oldname)
  `(define-abbreviation (,newname . parameters)
     `(,',oldname . ,parameters) ) )
(define-abbreviation (define-alias newname oldname)
  `(define-abbreviation (,newname . parameters)
     (cons ',oldname parameters) ) )

(define-abbreviation (generate-vector-of-fix-makers n)
  (let* ((numbers (iota 0 n))
         (variables (map (lambda (i) (gensym)) numbers)) )
    `(case size
       ,@(map (lambda (i) 
                (let ((vars (list-tail variables (- n i))))
                  `((,i) (lambda ,vars (vector cn . ,vars))) ) )
              numbers )
       (else #f) ) ) )              

(define-abbreviation (ifn condition consequent . alternant)
  (ifn (memq condition '(#t #f))
       `(if ,condition (ifn ,(null? alternant)
                            (begin . ,alternant)
                            (if #f 42) )
            ,consequent )
       (if condition consequent `(begin . ,alternant)) ) )

;;; Excerpts from chap9.bk

(define-abbreviation (with-gensym variables . body)
  `(let ,(map (lambda (var) `(,var (gensym)))
              variables )
     . ,body ) )

;;; exercice 1

(define-syntax repeat1
  (syntax-rules (:while :unless :do)
    ((_ :while p :unless q :do body ...)
     (let loop ()
       (if p (begin (if (not q) (begin body ...))
                    (loop) )) ) ) ) )

;;; end of chap9a.scm
