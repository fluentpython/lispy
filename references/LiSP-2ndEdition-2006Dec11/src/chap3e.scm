;;; $Id: chap3e.scm,v 4.0 1995/07/10 06:51:13 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

(define (fact n)
  (let ((r 1))
    (let ((k (call/cc (lambda (c) c))))
      (set! r (* r n))
      (set! n ( - n 1))
      (if (= n 1) r (k k)) ) ) )

(define-syntax catch 
  (syntax-rules
    ((catch tag . body)
     (let ((saved-catchers *active-catchers*))
       (unwind-protect 
         (block label
           (set! *active-catchers* 
                 (cons (cons tag (lambda (x) (return-from label x)))
                       *active-catchers* ) )
           . body )
         (set! *active-catchers* saved-catchers) ) ) ) ) )

(define-syntax let/cc 
  (syntax-rules ()
    ((let/cc variable . body)
     (block variable
       (let ((variable (lambda (x) (return-from variable x))))
         . body ) ) ) ) )

;;; end of chap3e.scm
