;;; $Id: chap10i.scm,v 4.0 1995/07/10 06:50:43 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Variants of chap10e.scm  			(untested)
;;; This works but SCM_invoke has to be modified to take this into account
;;; when invoking a fixed arity closure. Also encapsulate as in chap10e
;;; C details in C macros.

;;; Generates two entry points for functions.

(define (generate-functions out definitions)
  (format out "~%/* Functions: */~%")
  (for-each (lambda (def) 
              (generate-closure-structure out def)
              (generate-direct-definition out def)
              (generate-general-definition out def) )
            (reverse definitions) ) )

;;; C generation: Generate the struct only if it contains free
;;; variables.

(define (generate-closure-structure out definition)
  (when (Free-Environment? (Function-Definition-free definition))
    (format out "struct function_~A {~% enum SCM_tag header;~%"
            (Function-Definition-index definition) )
    (format out "SCM (*behavior)();~% int arity;")
    (let loop ((free (Function-Definition-free definition)))
      (when (Free-Environment? free)
        (format out "~%SCM ")
        (let ((freevarname 
               (Variable-name 
                (Reference-variable 
                 (Free-Environment-first free) ) ) ))
          (format out "~A;" freevarname) )
        (loop (Free-Environment-others free)) ) )
    (format out "~%};~%") ) )

;;; Extract the arguments from the va_list and just calls the other
;;; more direct entry point. The closure itself is added as last
;;; argument although received as first argument. This is useful
;;; since C seems to ignore extra arguments.

(define (generate-general-definition out definition)
  (if (Free-Environment? (Function-Definition-free definition))
      (format out "~%SCM_DeclareFunction(function_~A) {~%"
              (Function-Definition-index definition) )
      (format out "~%SCM_DeclareCombinator(function_~A) {~%"
              (Function-Definition-index definition) ) )
  (let ((vars (Function-Definition-variables definition))
        (rank -1) )
    (for-each (lambda (v)
                (set! rank (+ rank 1))
                (cond ((Local-Variable-dotted? v)
                       (format out "SCM_DeclareLocalDottedVariable(") )
                      ((Variable? v)
                       (format out "SCM_DeclareLocalVariable(") ) )
                (variable->C v out)
                (format out ",~A);~%" rank) )
              vars )
    (format out "return function_~A_("
            (Function-Definition-index definition) )
    (let loop ((vars vars))
      (when (pair? vars)
        (variable->C (car vars) out)
        (when (pair? (cdr vars))
          (format out ",")
          (loop (cdr vars)) ) ) )
    (when (Free-Environment? (Function-Definition-free definition))
      (when (pair? vars) (format out ", "))
      (format out "self_") )
    (format out ");~%}~%~%") ) )

(define (generate-direct-definition out definition)
  (format out "~%SCM function_~A_ ("
          (Function-Definition-index definition) )
  (let loop ((vars (Function-Definition-variables definition)))
      (when (pair? vars)
        (format out "SCM ")
        (variable->C (car vars) out)
        (when (pair? (cdr vars))
          (format out ", ")
          (loop (cdr vars)) ) ) )
  (when (Free-Environment? (Function-Definition-free definition))
    (when (pair? (Function-Definition-variables definition))
      (format out ", ") )
    (format out "struct function_~A *self_"
            (Function-Definition-index definition) ) )
  (format out ") {~%")
  (let ((temps (With-Temp-Function-Definition-temporaries definition)))
    (when (pair? temps) 
      (generate-local-temporaries temps out)
      (format out "~%") ) )
  (format out "return ")
  (->C (Function-Definition-body definition) out)
  (format out ";~%}~%") )

;;; end of chap10i.scm
