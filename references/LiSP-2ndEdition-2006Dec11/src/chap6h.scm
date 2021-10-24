;;; $Id: chap6h.scm,v 4.0 1995/07/10 06:51:46 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Improvement on chap6d.scm for niladic functions.

(define (meaning-fix-abstraction n* e+ r tail?)
  (let ((arity (length n*)))
    (if (= arity 0)
        (let ((m+ (meaning-sequence e+ r #t)))
          (THUNK-CLOSURE m+) )
        (let* ((r2 (r-extend* r n*))
               (m+ (meaning-sequence e+ r2 #t)) )
          (FIX-CLOSURE m+ arity) ) ) ) )

(define (THUNK-CLOSURE m+)
  (let ((arity+1 (+ 0 1)))
    (lambda ()
      (define (the-function v* sr)
        (if (= (activation-frame-argument-length v*) arity+1)
            (begin (set! *env* sr)
                   (m+) )
            (wrong "Incorrect arity") ) )
      (make-closure the-function *env*) ) ) )

;;; end of chap6h.scm
