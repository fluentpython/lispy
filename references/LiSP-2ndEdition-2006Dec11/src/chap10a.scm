;;; $Id: chap10a.scm,v 4.2 2006/11/25 17:44:38 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; This file converts a Sexp into a Program object. This object will
;;; be later used by different analyzes all done with oo-technology
;;; married with code-walking technology. 

;;; Variables are not Programs! They represent bindings.

(define-class Variable Object (name))

(define-class Global-Variable Variable ())

(define-class Predefined-Variable Variable (description))

(define-class Local-Variable Variable (mutable? dotted?))

;;; Descriptions

(define-class Functional-Description Object 
  (comparator arity generator) )

(define-class Constant-Description Object (value))



(define-class Program Object ())

;;; References are programs.

(define-class Reference Program (variable))

(define-class Local-Reference Reference ())

(define-class Global-Reference Reference ())

(define-class Predefined-Reference Reference ())

(define-class Local-Assignment Program (reference form))

(define-class Global-Assignment Program (variable form))

(define-class Box-Read Program (reference))

(define-class Box-Write Program (reference form))

(define-class Box-Creation Program (variable))

(define-class Function Program (variables body))

(define-class Alternative Program (condition consequent alternant))

(define-class Sequence Program (first last))

(define-class Constant Program (value) )

(define-class Application Program ())

(define-class Regular-Application Application (function arguments))

(define-class Predefined-Application Application (variable arguments))

(define-class Fix-Let Program (variables arguments body))

(define-class Arguments Program (first others))

(define-class No-Argument Program ())

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; No syntactic checks here.

(define (objectify e r)
  (if (atom? e)
      (if (symbol? e)
          (objectify-reference e r)
          (objectify-quotation e r) ) 
      (case (car e)
        ((quote)  (objectify-quotation (cadr e) r))
        ((if)     (objectify-alternative (cadr e) (caddr e) (cadddr e) r))
        ((begin)  (objectify-sequence (cdr e) r))
        ((set!)   (objectify-assignment (cadr e) (caddr e) r))
        ((lambda) (objectify-function (cadr e) (cddr e) r))
        (else     (objectify-application (car e) (cdr e) r)) ) ) )

(define (objectify-quotation value r)
  (make-Constant value) )

(define (objectify-alternative ec et ef r)
  (make-Alternative (objectify ec r)
                    (objectify et r)
                    (objectify ef r) ) )
;;; NOTE: In Scheme, alternatives may miss the alternant part.

(define (objectify-sequence e* r)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (let ((a (objectify (car e*) r)))
            (make-Sequence a (objectify-sequence (cdr e*) r)) )
          (objectify (car e*) r) )
      (make-Constant 42) ) )

(define (objectify-application f e* r)
  (let ((ff  (objectify f r))
        (ee* (convert2arguments (map (lambda (e) (objectify e r)) e*))) )
    (cond ((Function? ff)  
           (process-closed-application ff ee*) )
          ((Predefined-Reference? ff) 
           (let* ((fvf (Predefined-Reference-variable ff))
                  (desc (Predefined-Variable-description fvf)) )
             (if (Functional-Description? desc)
                 (if ((Functional-Description-comparator desc)
                      (length e*) (Functional-Description-arity desc) )
                     (make-Predefined-Application fvf ee*)
                     (objectify-error "Incorrect predefined arity" f e*) )
                 (make-Regular-Application ff ee*) ) ) )
          (else (make-Regular-Application ff ee*)) ) ) )

(define (process-closed-application f e*)
  (let ((v* (Function-variables f))
        (b  (Function-body f)) )
    (if (and (pair? v*) (Local-Variable-dotted? (car (last-pair v*))))
        (process-nary-closed-application f e*) 
        (if (= (number-of e*) (length v*))
            (make-Fix-Let v* e* b)
            (objectify-error "Incorrect regular arity" f e*) ) ) ) )

(define (process-nary-closed-application f e*)
   (let* ((v* (Function-variables f))
          (b  (Function-body f))
          (o (make-Fix-Let 
              v*
              (let gather ((e* e*) (v* v*))
                (if (Local-Variable-dotted? (car v*))
                    (make-Arguments 
                     (let pack ((e* e*))
                       (if (Arguments? e*)
                           (make-Predefined-Application
                            (find-variable? 'cons g.init)
                            (make-Arguments 
                             (Arguments-first e*) 
                             (make-Arguments 
                              (pack (Arguments-others e*)) 
                              (make-No-Argument) ) ) )
                           (make-Constant '()) ) )
                     (make-No-Argument) )
                    (if (Arguments? e*)
                        (make-Arguments (Arguments-first e*) 
                                        (gather (Arguments-others e*) 
                                                (cdr v*) ) )
                        (objectify-error "Incorrect dotted arity" f e*) ) ) )
              b )) )
     (set-Local-Variable-dotted?! (car (last-pair v*)) #f)
     o ) )
 
(define (convert2arguments e*)
  (if (pair? e*)
      (make-Arguments (car e*) (convert2arguments (cdr e*)))
      (make-No-Argument) ) )

(define-generic (number-of (o)))

(define-method (number-of (o Arguments))
  (+ 1 (number-of (Arguments-others o))) )

(define-method (number-of (o No-Argument)) 0)

;;; Assignment on mutable local variables are immediately
;;; processed. References to local mutable variables must wait the
;;; second pass.

(define (objectify-assignment variable e r)
  (let ((ov (objectify variable r))
        (of (objectify e r)) )
    (cond ((Local-Reference? ov)
           (set-Local-Variable-mutable?! (Local-Reference-variable ov) #t)
           (make-Local-Assignment ov of) )
          ((Global-Reference? ov)
           (make-Global-Assignment (Global-Reference-variable ov) of) )
          ((Predefined-Reference? ov)
           (objectify-error "Predefined are immutable" variable)) ) ) )

;;; It is important that the body is objectified first, so mutability
;;; of local variables is known and they can be appropriately handled
;;; in objectify-variables-list. 
;;; All variables are considered immutable at the beginning.

(define (objectify-function names body r)
  (let* ((vars (objectify-variables-list names))
         (b    (objectify-sequence body (r-extend r vars))) )
    (make-Function vars b) ) )

(define (r-extend r vars)
  (append vars r) )

(define (objectify-variables-list names)
  (if (pair? names)
      (cons (make-Local-Variable (car names) #f #f)
            (objectify-variables-list (cdr names)) )
      (if (symbol? names)
          (list (make-Local-Variable names #f #t))
          '() ) ) )

(define (objectify-reference variable r)
  (let ((v (or (find-variable? variable r)
               (find-variable? variable g.current)
               (find-variable? variable g.init) )))
    (cond ((Local-Variable? v)      (make-Local-Reference v))
          ((Global-Variable? v)     (make-Global-Reference v))
          ((Predefined-Variable? v) (make-Predefined-Reference v))
          (else (objectify-free-global-reference variable r)) ) ) )

(define (objectify-free-global-reference variable r)
  (objectify-error "No such variable" variable) )

(define (find-variable? name r)
  (if (pair? r)
      (if (eq? name (Variable-name (car r)))
          (car r)
          (find-variable? name (cdr r)) )
      #f ) )

;;; This variable will contain the description of the predefined
;;; global environment. This environment is defined in chap10h.scm

(define g.init '())

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; To ease the writing of the code-walker, anything in a program has to
;;; be an object so we can apply meta-methods.

(define (update-walk! g o . args)
  (for-each (lambda (field)
              (let ((vf (field-value o field)))
                (when (Program? vf)
                  (let ((v (if (null? args) (g vf) 
                               (apply g vf args) )))
                    (set-field-value! o v field) ) ) ) )                  
            (Class-fields (object->class o)) )
  o )

(define-generic (insert-box! (o Program))
  (update-walk! insert-box! o) )

(define-method (insert-box! (o Local-Reference))
  (if (Local-Variable-mutable? (Local-Reference-variable o))
      (make-Box-Read o) 
      o ) )

(define-method (insert-box! (o Local-Assignment))
  (make-Box-Write (Local-Assignment-reference o)
                  (insert-box! (Local-Assignment-form o)) ) )

;(define-method (insert-box! (o Box-Write))
;  (set-Box-Write-form! o (insert-box! (Box-Write-form o)))
;  o )

(define-method (insert-box! (o Fix-Let))
  (set-Fix-Let-arguments! o (insert-box! (Fix-Let-arguments o)))
  (set-Fix-Let-body! 
   o (insert-box!
      (boxify-mutable-variables (Fix-Let-body o) 
                                (Fix-Let-variables o) ) ) )
  o )

(define-method (insert-box! (o Function))
  (set-Function-body!
   o (insert-box!
      (boxify-mutable-variables (Function-body o) 
                                (Function-variables o) ) ) )
  o ) 

(define (boxify-mutable-variables form variables)
  (if (pair? variables)
      (if (Local-Variable-mutable? (car variables))
          (make-Sequence 
           (make-Box-Creation (car variables))
           (boxify-mutable-variables form (cdr variables)) )
          (boxify-mutable-variables form (cdr variables)) )
      form ) )  

;;; Second pass. Use the collected information on mutability of
;;; variables to update all Local-References to pass through a box to
;;; read a variable value. It is already done for local assignments.

(define (Sexp->object exp)
  (insert-box! (objectify exp '())) )

;;; Global variables

(define objectify-error 'wait)
(define evaluate-error 'wait)

;;; end of chap10a.scm
