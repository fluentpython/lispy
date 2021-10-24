;;; $Id: chap10c.scm,v 4.4 2006/11/27 12:13:37 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;;                 More passes in the compiler.

(define-class Free-Reference Reference ())

(define-class Flat-Function Function (free))

(define-class Free-Environment Program (first others))

(define-class No-Free Program ())

(define-class Flattened-Program Program 
  (form quotations definitions) )

;;; Keep the variables to compute the arity of the function when generating C.

(define-class Closure-Creation Program (index variables free))

(define-class Function-Definition Flat-Function (index))

;;; Its name is a number. The value field is the quoted value.

(define-class Quotation-Variable Variable (value))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; lifting transformation. Identifies free variables and
;;; abstractions.  Pay attention to walk the expression in prefix
;;; order so that references in C code are in the right order.

(define (lift! o)
  (lift-procedures! o #f '()) )

;;; The code walker. Walk O, store free variables in FLATFUN (the
;;; closest abstraction), bound variables appear in VARS.

(define-generic (lift-procedures! (o Program) flatfun vars)
  (update-walk! lift-procedures! o flatfun vars) )

;;; Collect free variables into flatfun.

(define-method (lift-procedures! (o Local-Reference) flatfun vars)
  (let ((v (Local-Reference-variable o)))
    (if (memq v vars)
        o (begin (adjoin-free-variable! flatfun o)
                 (make-Free-Reference v) ) ) ) )

;;; CPS tend to share structures, so lift! must be able to handle these.
;;; This was a patch, letify in chap10k now copies the AST to remove these
;;; undesirable sharing.
;(define-method (lift-procedures! (o Free-Reference) flatfun vars)
;  (let ((v (Free-Reference-variable o)))
;    (if (memq v vars)
;        (make-Local-Reference v)
;        (begin (adjoin-free-variable! flatfun o)
;               o ) ) ) )

;;; Transform functions into flat-functions. Don't forget that free
;;; variables of the newfun abstraction may be free in flatfun.

(define-method (lift-procedures! (o Function) flatfun vars)
  (let* ((localvars (Function-variables o))
         (body   (Function-body o))
         (newfun (make-Flat-Function 
                  localvars body (make-No-Free) )) )
    (set-Flat-Function-body! 
     newfun (lift-procedures! body newfun localvars) )
    (let ((free* (Flat-Function-free newfun)))
      (set-Flat-Function-free! 
       newfun (lift-procedures! free* flatfun vars) ) )
    newfun ) )

;;; Fix-Let is also a binding form that may shadow free variables.

(define-method (lift-procedures! (o Fix-Let) flatfun vars)
  (set-Fix-Let-arguments!
   o (lift-procedures! (Fix-Let-arguments o) flatfun vars) )
  (let ((newvars (append (Fix-Let-variables o) vars)))
    (set-Fix-Let-body! 
     o (lift-procedures! (Fix-Let-body o) flatfun newvars) )
    o ) )

;;; Adjoin a free variable if not already there.

(define (adjoin-free-variable! flatfun ref)
  (when (Flat-Function? flatfun)
    (let check ((free* (Flat-Function-free flatfun)))
      (if (No-Free? free*)
          (set-Flat-Function-free! 
           flatfun (make-Free-Environment 
                    ref (Flat-Function-free flatfun) ) )
          (unless (eq? (Reference-variable ref)
                       (Reference-variable 
                        (Free-Environment-first free*) ) )
            (check (Free-Environment-others free*)) ) ) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Collect all functions and quotations and store them in the fieds
;;; of a Flattened-Program object. Avoid global variables and store
;;; directly in the result.

(define (extract-things! o)
  (let ((result (make-Flattened-Program o '() '())))
    (set-Flattened-Program-form! result (extract! o result))
    result ) )

(define-generic (extract! (o Program) result)
  (update-walk! extract! o result) )

(define-method (extract! (o Constant) result)
  (let* ((qv* (Flattened-Program-quotations result))
         (qv  (make-Quotation-Variable (length qv*) 
                                       (Constant-value o) )) )
    (set-Flattened-Program-quotations! result (cons qv qv*))
    (make-Global-Reference qv) ) )

(define-method (extract! (o Flat-Function) result)
  (let* ((newbody   (extract! (Flat-Function-body o) result))
         (variables (Flat-Function-variables o))
         (freevars  (let extract ((free (Flat-Function-free o)))
                      (if (Free-Environment? free)
                          (cons (Reference-variable 
                                 (Free-Environment-first free) )
                                (extract 
                                 (Free-Environment-others free) ) )
                          '() ) ))
         (index (adjoin-definition! 
                 result variables newbody freevars )) )
    (make-Closure-Creation 
     index variables (Flat-Function-free o) ) ) )

;;; May try to share Function-Definitions.

(define (adjoin-definition! result variables body free)
  (let* ((definitions (Flattened-Program-definitions result))
         (newindex (length definitions)) )
    (set-Flattened-Program-definitions! 
     result (cons (make-Function-Definition
                   variables body free newindex )
                  definitions ) )
    newindex ) )

;;; end of chap10c.scm
