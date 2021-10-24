;;; $Id: variante1.scm,v 1.5 2006/11/25 17:44:13 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; This file is part of the Meroonet package.

;;; VARIANT 1:  a simple-minded define-class

;;; a define-class that works only for interpreters, the class is
;;; created at macroexpansion time and grafted to the inheritance tree
;;; at that same time. The version in Meroonet is safer.

(define-meroonet-macro (define-class name super-name 
                                     own-field-descriptions )
  (let ((class (register-class name super-name 
                               own-field-descriptions )))
    (Class-generate-related-names class) ) )

;;; end of variante1.scm
