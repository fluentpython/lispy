;;; $Id: chap9f.scm,v 4.0 1995/07/10 06:52:22 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Only one world

(define (make-macro-environment current-level)
  (let ((metalevel (delay current-level)))
    (list (make-Magic-Keyword 'eval-in-abbreviation-world 
           (special-eval-in-abbreviation-world metalevel) )
          (make-Magic-Keyword 'define-abbreviation 
           (special-define-abbreviation metalevel))
          (make-Magic-Keyword 'let-abbreviation    
           (special-let-abbreviation metalevel))
          (make-Magic-Keyword 'with-aliases   
           (special-with-aliases metalevel) ) ) ) )

;;; end of chap9f.scm
