;;; $Id: chap6g.tst,v 4.0 1995/07/10 06:51:45 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; test that only explicitely defined variables are created.

(define xfoo 3)
   ---
(define xfoo (lambda () xbar))
   ***
(begin (define xfoo (lambda () xbar))
       (define xbar 33) )
   ---
(begin (define xfoo (lambda () xbar))
       (define xbar (lambda () xhux)) )
   ***
(begin (define xfoo (lambda () xbar))
       (define xbar (lambda () (xhux))) )
   ***
