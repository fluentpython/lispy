;;; $Id: hooks.sc,v 1.2 1994/07/17 18:33:19 queinnec Exp $
;;; hooks.ss
;;; Robert Hieb & Kent Dybvig
;;; 92/06/18

;;;       adapted for Scheme->c by Christian Queinnec

(define eval-hook eval)

(define expand-install-hook 
  (lambda (expand) 
    (error-hook 'expand-install-hook "Not yet implemented" expand) ) )

;;; In Chez Scheme, the following reports:
;;;           "Error in <who>: <why> <what>."
;;; "who" is a symbol, "why" is a string, and "what" is an arbitrary object.

(define error-hook 
  (lambda (who why what)
    (error who " ~A: ~A.~%" why what) ) )

;;; New symbols are used to generate non-capturing bindings.  If it is
;;; impossible to generate unique symbols, output identifiers during
;;; expansion and either feed the result directly into the compiler or
;;; make another pass to perform alpha substitution.

;;; gensym does not exist in Scheme->C and has to be defined before
;;; this assignment since no error will be perceived in compiled code.

(define new-symbol-hook gensym)

;;; "put-global-definition-hook" should overwrite existing definitions.

(define put-global-definition-hook
   (lambda (symbol binding)
      (putprop symbol '*macro-transformer* binding)))

;;; "get-global-definition-hook" should return "#f" if no binding
;;; has been established "put-global-definition-hook" for the symbol.

(define get-global-definition-hook
   (lambda (symbol)
      (getprop symbol '*macro-transformer*)))

;;; end of hooks.sc
