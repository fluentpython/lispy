;;; $Id: chap10h.scm,v 4.1 2006/11/25 17:44:43 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Define an initial environment with the predefined primitives. Only
;;; those that have a fixed number of arguments appear. Since direct
;;; calls are emitted to them only if they have a fixed arity.

(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name Cname arity)
     (let ((v (make-Predefined-Variable 
               'name (make-Functional-Description 
                      = arity 
                      (make-predefined-application-generator 
                       'Cname ) ) )))
       (set! g.init (cons v g.init))
       'name ) ) ) )

(defprimitive cons "SCM_cons" 2)
(defprimitive car "SCM_car" 1)
(defprimitive cdr "SCM_cdr" 1)
(defprimitive set-car! "SCM_set_car" 2)
(defprimitive set-cdr! "SCM_set_cdr" 2)
(defprimitive pair? "SCM_pairp" 1)
(defprimitive null? "SCM_nullp" 1)
(defprimitive symbol? "SCM_symbolp" 1)
(defprimitive stringp "SCM_stringp" 1)
(defprimitive eq? "SCM_eqp" 2)
(defprimitive integer? "SCM_fixnump" 1)
(defprimitive procedure? "SCM_procedurep" 1)

;;; Use macros instead of calling functions. 

(defprimitive + "SCM_Plus" 2)
(defprimitive - "SCM_Minus" 2)
(defprimitive * "SCM_Times" 2)
(defprimitive / "SCM_Quotient" 2)
(defprimitive remainder "SCM_Remainder" 2)
(defprimitive <= "SCM_LeP" 2)
(defprimitive >= "SCM_GeP" 2)
(defprimitive = "SCM_EqnP" 2)
(defprimitive < "SCM_LtP" 2)
(defprimitive > "SCM_GtP" 2)

(defprimitive call/ep "SCM_callep" 1)
(defprimitive print "SCM_print" 1)

;;; Not really true since SCM_prin is only monadic whereas display may
;;; have an optional port:

(defprimitive display "SCM_prin" 1)

;;; Define as well some global constants

(define-syntax definitial
  (syntax-rules ()
    ((definitial name value)
     (let ((v (make-Predefined-Variable 
               'name (make-Constant-Description value) )))
       (set! g.init (cons v g.init))
       'name ) ) ) )

(definitial t   "SCM_true")
(definitial f   "SCM_false")
(definitial nil "SCM_nil")

;;; Apply is a special case. It has to be invoked specially as an nary
;;; subr.  It is not inlined.

(begin
  (set! g.init (append (map (lambda (name) 
                              (make-Predefined-Variable name #f) )
                            '(list apply call/cc) )
                       g.init ))
  'apply )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; This is very restrictive and can be extended.

(define-syntax defforeignprimitive
  (syntax-rules (int string)
    ((defforeignprimitive name int (Cname string) arity)
     (let ((v (make-Predefined-Variable 
               'name (make-Functional-Description 
                      = arity 
                      (lambda (e out)
                        (format out "SCM_Int2fixnum")
                        (between-parentheses out
                          (format out "~A" Cname)
                          (between-parentheses out
                            (arguments->C (Predefined-Application-arguments e)
                                          out ) ) ) ) ) )))
       (set! g.init (cons v g.init))
       'name ) ) ) )

(defforeignprimitive system int ("system" string) 1)

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; This is used to generate parts of the scheme.{c,h} files.

;(define (generate-prototypes g out)
;  (for-each (lambda (gv)
;              (let ((desc (Predefined-Variable-description gv)))
;                (cond ((Functional-Description? desc)
;                       (format out "SCM_DefinePredefinedVariable(")
;                       (variable->C gv out)
;                       (format out ",\"~A\",~A,~A);~%"
;                               (Variable-name gv)
;                               (Functional-Description-arity desc)
;                               (Functional-Description-Cname desc) ) )
;                      ((Constant-Description? desc)
;                       (format out "SCM_DefineInitializedGlobalVariable(")
;                       (variable->C gv out)
;                       (format out ",\"~A\",~A);~%"
;                               (Variable-name gv)
;                               (Constant-Description-value desc) ) ) ) ) )
;            g ) )
;;; (generate-prototypes g.init (current-output-port))

;(define (generate-declarations g out)
;  (for-each (lambda (gv)
;              (let ((desc (Predefined-Variable-description gv)))
;                (cond ((Functional-Description? desc)
;                       (format out "SCM_DeclareSubr~A("
;                               (Functional-Description-arity desc) )
;                       (variable->C gv out)
;                       (format out ",~A);~%" 
;                               (Functional-Description-Cname desc) ) )
;                      ((Constant-Description? desc)
;                       (format out "SCM_DeclareConstant(")
;                       (variable->C gv out)
;                       (format out ");~%") ) ) ) )
;            g ) )
;;; (generate-declarations g.init (current-output-port))

;;; end of chap10h.scm
