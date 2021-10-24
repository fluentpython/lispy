;;; $Id: chap9z.scm,v 4.2 2006/11/25 17:01:28 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Various excerpts for chapter on macros

(define (prepare expression directives)
  (let ((macroexpand (generate-macroexpand directives)))
    (really-prepare (macroexpand expression)) ) )

(define (prepare expression)
  (really-prepare (macroexpand expression)) )

(define-abbreviation (while condition . body)         \[\hfill\em{LOOP}\]
  `(if ,condition (begin (begin . ,body) 
                         (while ,condition . ,body) )) )

(define-abbreviation (incredible x)               \[\hfill\em{BAD TASTE}\]
  (call/cc (lambda (k) `(quote (,k ,x)))) )

(define-abbreviation (define-immediate-abbreviation call . body)
  (let ((name (gensym)))
    `(begin (define ,name (lambda ,(cdr call) . ,body))
            (define-abbreviation ,call (,name . ,(cdr call))) ) ) )

;;; end of chap9z.scm
