;;; $Id: chap3b.scm,v 4.2 2005/07/19 09:20:44 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

(define-syntax throw 
  (syntax-rules ()
    ((throw tag value)
     (let* ((label tag)                                 ; compute once
            (escape (assv label (dynamic *active-catchers*))) )
       (if (pair? escape)
           ((cdr escape) value)
           (wrong "No associated catch to" label) ) ) ) ) )

(define-syntax catch 
  (syntax-rules ()
    ((catch tag . body)
     (block label
       (dynamic-let ((*active-catchers* 
                      (cons (cons tag (lambda (x) 
                                        (return-from label x) ))
                            (dynamic *active-catchers*) ) ))
           . body ) ) ) ) )  

(define (find-symbol id tree)
  (define (find tree)
     (if (pair? tree)
         (or (find (car tree))
             (find (cdr tree)) )
         (if (eq? tree id) (throw 'find #t) #f) ) )
  (catch 'find (find tree)) )


;;; end of chap3b.scm
