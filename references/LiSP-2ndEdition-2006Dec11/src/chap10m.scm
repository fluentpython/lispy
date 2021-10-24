;;; $Id: chap10m.scm,v 4.0 1995/07/10 06:50:50 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; The letify generic function that take an AST with possible sharing
;;; and copies it in a pure tree, trying at the same time to insert
;;; let forms where possible. 

;;; Only local variables are cloned. Global or predefined variables
;;; stay the same (and are shared between the input and the result of
;;; letify).

(define-generic (letify (o Program) env)
  (update-walk! letify (clone o) env) )

;;; Tell how to clone Variables. Not useful for the following:
;;; Global-Variable
;;; Predefined-Variable
;;; Local-Variable
;;; Quotation-Variable
;;; Renamed-Local-Variable   (introduced by gather-temporaries)
;;; Global-Variable

(define-method (clone (o Pseudo-Variable))
  (new-Variable) )

;;; If letify is used in other places, it might be interesting to add
;;; these methods.

;(define-method (clone (o Renamed-Local-Variable))
;  (new-renamed-variable o) )

;(define-method (clone (o Local-Variable))
;  (new-renamed-variable o) )

(define-method (letify (o Function) env)
  (let* ((vars (Function-variables o))
         (body (Function-body o))
         (new-vars (map clone vars)) )
    (make-Function 
     new-vars
     (letify body (append (map cons vars new-vars) env)) ) ) )

;;; Don't preserve continuations as they are.

;;; Other types of references:
;;; Global-Reference
;;; Predefined-Reference
;;; Free-Reference        (introduced by lift!).

(define-method (letify (o Local-Reference) env)
  (let* ((v (Local-Reference-variable o))
         (r (assq v env)) )
    (if (pair? r) 
        (make-Local-Reference (cdr r))
        (letify-error "Disappeared variable" o) ) ) )

(define-method (letify (o Regular-Application) env)
  (if (Function? (Regular-Application-function o))
      (letify (process-closed-application 
               (Regular-Application-function o)
               (Regular-Application-arguments o) )
              env )
      (make-Regular-Application
       (letify (Regular-Application-function o) env)
       (letify (Regular-Application-arguments o) env) )  ) )

(define-method (letify (o Fix-Let) env)
  (let* ((vars (Fix-Let-variables o))
         (new-vars (map clone vars)) )
    (make-Fix-Let
     new-vars
     (letify (Fix-Let-arguments o) env)
     (letify (Fix-Let-body o) 
             (append (map cons vars new-vars) env) ) ) ) )

(define-method (letify (o Box-Creation) env)
  (let* ((v (Box-Creation-variable o))
         (r (assq v env)) )
    (if (pair? r)
        (make-Box-Creation (cdr r))
        (letify-error "Disappeared variable" o) ) ) )

;;; end of chap10m.scm
