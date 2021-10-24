;;; $Id: chap9b.scm,v 4.0 1995/07/10 06:52:16 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

(define-abbreviation (define-abbreviation call . body)
  `(install-macro! ',(car call) (lambda ,(cdr call) . ,body)) )

(define-abbreviation (define-meroonet-macro call . body)
  `(begin (define-abbreviation ,call . ,body)
          (eval '(define-abbreviation ,call . ,body)) ) )

(define-abbreviation (meroon-if condition consequent . alternant)
  `(if (let ((tmp ,condition))
         (or tmp (null? tmp)) )
       ,consequent . ,alternant ) )

(define-abbreviation (define-inline call . body)
  (let ((name      (car call))
        (variables (cdr call)) )
    `(begin 
       (define-abbreviation (,name . arguments)
         (cons (cons 'lambda (cons ',variables ',body))
               arguments ) )
       (define ,call (,name . ,variables)) ) ) )

(define-syntax enumerate
  (syntax-rules ()
    ((enumerate) (display 0))
    ((enumerate e1 e2 ...)
     (begin (display 0) (enumerate-aux e1 (e1) e2 ...) ) ) ) )

(define-syntax enumerate-aux 
  (syntax-rules ()
    ((enumerate-aux e1 len) (begin e1 (display (length 'len))))
    ((enumerate-aux e1 len e2 e3 ...)
     (begin e1 (display (length 'len))
            (enumerate-aux e2 (e2 . len) e3 ...) ) ) ) )

;;; end of chap9b.scm
