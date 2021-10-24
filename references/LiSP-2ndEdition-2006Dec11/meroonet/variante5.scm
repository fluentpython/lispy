;;; $Id: variante5.scm,v 1.3 1996/01/28 17:29:37 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; This file is part of the Meroonet package.

;;; VARIANT 5:             next-method?

(define-meroonet-macro (define-method call . body)
  (parse-variable-specifications
   (cdr call)
   (lambda (discriminant variables)
     (let ((g (gensym))(c (gensym)))    ; make g and c hygienic
       `(register-method
         ',(car call)
         (lambda (,g ,c)
           (lambda ,(flat-variables variables)
             ,@(generate-next-method-functions g c variables)
             . ,body ) )
         ',(cadr discriminant)
         ',(cdr call) ) ) ) ) )

;;; It would be better to generate these functions only if the
;;; identifiers call-next-method or next-method? occurs somewhere in
;;; the body of the method.

(define (generate-next-method-functions g c variables)
  (let ((get-next-method (gensym)))
    `((define (,get-next-method)
        (if (Class-superclass ,c)
            (vector-ref (Generic-dispatch-table ,g) 
                        (Class-number (Class-superclass ,c)) )
            (Generic-default ,g) ) )
      (define (call-next-method)
        ((,get-next-method) . ,(flat-variables variables)) )
      (define (next-method?)
        (not (eq? (,get-next-method) (Generic-default ,g))) ) ) ) )

;;; Testing next-method?

(define-class Point Object (x y))

(define-class ColoredPoint Point (color))

(define-generic (g1 (a)))

(define-generic (g2 (a))
  'g2 )

(define-method (g1 (a Point))
  (next-method?) )

(define-method (g2 (a Point))
  (next-method?) )

(define-method (g1 (a ColoredPoint))
  (next-method?) )

(define-method (g2 (a ColoredPoint))
  (next-method?) )

(unless (let ((pt (make-Point 11 22))
              (cpt (make-ColoredPoint 33 44 'orange)) )
          (and (not (g1 pt))
               (not (g2 pt))
               (g1 cpt)
               (g2 cpt) ) )
  (meroonet-error "Failed tests on next-method?") )

;;; end of variante5.scm
