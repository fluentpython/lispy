;;; $Id: variante3.scm,v 1.2 1994/08/21 19:35:09 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; This file is part of the Meroonet package.

;;; VARIANT 3:             More reflective classes

;;; First make generic some functions so they can be specialized on
;;; new metaclasses.  Then make Meroonet use these new functions
;;; instead.

(define-generic (generate-related-names (class)))

(define-method (generate-related-names (class Class))
   (Class-generate-related-names class) )

;;; The other initialization parameters must be given in correct order.

(define-generic (initialize! (o) . args))

(define-method (initialize! (o Class) . args)
  (apply Class-initialize! o args) )

;;; Define new metaclass with additional slots to hold the predicate,
;;; the allocator and the maker of the class.

(define-class ReflectiveClass Class (predicate allocator maker))

(define-method (generate-related-names (class ReflectiveClass))
  (let ((cname (symbol-append (Class-name class) '-class))
        (predicate-name (symbol-append (Class-name class) '?))
        (allocator-name (symbol-append 'allocate- (Class-name class)))
        (maker-name (symbol-append 'make- (Class-name class))) )
    `(begin ,(call-next-method)
            (set-ReflectiveClass-predicate! ,cname ,predicate-name)
            (set-ReflectiveClass-allocator! ,cname ,allocator-name)
            (set-ReflectiveClass-maker! ,cname ,maker-name) ) ) )

;;; To test it, we redefine define-class to use ReflectiveClass
;;; instead of Class.

(define-meroonet-macro (define-class name super-name own-field-descriptions)
  (let ((class (register-ReflectiveClass
                name super-name own-field-descriptions )))
    (generate-related-names class) ) )

(define (register-ReflectiveClass name super-name own-field-descriptions)
  (initialize! (allocate-ReflectiveClass) 
               name
               (->Class super-name) 
               own-field-descriptions ) )

;;; Test a little the previous metaclass

(define-class Point Object (x y))

(unless (let ((pt (make-Point 11 22)))
          (and (procedure? (ReflectiveClass-maker Point-class))
               ((ReflectiveClass-predicate Point-class) pt)
               (equal? pt ((ReflectiveClass-maker Point-class) 11 22)) ) )
  (meroonet-error "Failed tests on ReflectiveClass") )
