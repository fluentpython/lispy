;;; $Id: chap1c.scm,v 4.0 1995/07/10 06:50:54 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;;                  variants of chapter 1.

(define (s.make-function variables body env)
  (lambda (values current.env)
    (let ((old-bindings
           (map (lambda (var val) 
                      (let ((old-value (getprop var 'apval)))
                        (putprop var 'apval val)
                        (cons var old-value) ) )
                variables
                values ) ))
      (let ((result (eprogn body current.env)))
        (for-each (lambda (b) (putprop (car b) 'apval (cdr b)))
                  old-bindings )
        result ) ) ) )

(define (s.lookup id env)
  (getprop id 'apval) )

(define (s.update! id env value)
  (putprop id 'apval value) ) 

;;; end of chap1c.scm
