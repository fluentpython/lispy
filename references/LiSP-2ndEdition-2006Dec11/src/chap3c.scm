;;; $Id: chap3c.scm,v 4.1 1996/02/16 19:28:34 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

(define-syntax block 
  (syntax-rules ()
    ((block label . body)
     (let ((label (list 'label)))
       (catch label . body) ) ) ) )

(define-syntax return-from
  (syntax-rules ()
    ((return-from label value)
     (throw label value) ) ) )

(define (find-symbol id tree)
  (block find
    (letrec ((find (lambda (tree)
                     (if (pair? tree)
                         (or (find (car tree))
                             (find (cdr tree)) )
                         (if (eq? id tree) (return-from find #t) 
                             #f ) ) )))
      (find tree) ) ) )

;;; end of chap3c.scm
