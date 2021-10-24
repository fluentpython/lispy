;;; $Id: variante4.scm,v 1.5 2006/11/25 17:43:56 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; This file is part of the Meroonet package.

;;; VARIANT 4:             enhanced(?) define-method

;;; This ne define-method a la CLOS creates a generic function on the
;;; fly if it does not exist yet. The sole problem is to do it a
;;; evaluation-time and not at macro-expansion time.

(define-meroonet-macro (define-method call . body)
  (parse-variable-specifications
   (cdr call)
   (lambda (discriminant variables)
     (let ((g (gensym))(c (gensym)))    ; make g and c hygienic
       `(begin
          (unless (->Generic ',(car call)) 
            (define-generic ,call) )        ; new
          (register-method
           ',(car call)
           (lambda (,g ,c)
             (lambda ,(flat-variables variables)
               (define (call-next-method)
                 ((if (Class-superclass ,c)
                      (vector-ref (Generic-dispatch-table ,g) 
                                  (Class-number 
                                   (Class-superclass ,c) ) )
                      (Generic-default ,g) )
                  . ,(flat-variables variables) ) )
               . ,body ) )
           ',(cadr discriminant)
           ',(cdr call) ) ) ) ) ) )

;;; I am not really fond of that but we can nevertheless test the
;;; previous definition.

(define-method (ugly a (b Class) . c)
  'barf )

(unless (eq? 'barf (ugly 2 Field-class))
  (meroonet-error "Failed test on define-method") )

;;; end of variante4.scm
