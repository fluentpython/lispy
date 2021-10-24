;;; $Id: chap10g.scm,v 4.1 1996/02/18 20:09:12 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

(define-class Renamed-Local-Variable Variable (index))

(define-class With-Temp-Function-Definition Function-Definition 
  (temporaries) )

;;;  Some additional code walkers before compiling to C.

;;; By the way, turn the expression to compile into a closure that is
;;; called from main.

(define (closurize-main! o)
  (let ((index (length (Flattened-Program-definitions o))))
    (set-Flattened-Program-definitions!
     o (cons (make-Function-Definition 
              '() (Flattened-Program-form o) '() index )
             (Flattened-Program-definitions o) ) )
    (set-Flattened-Program-form!
     o (make-Regular-Application
        (make-Closure-Creation index '() (make-No-Free))
        (make-No-Argument) ) )
    o ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Rename local bindings of Fix-Let and collect these bindings within
;;; Function-Definitions. This is necessary for C which do not provide
;;; the possibility to have an expression introducing local variables
;;; ie {int tmp ; ... } is an instruction without expression
;;; counterpart.

(define (gather-temporaries! o)
  (set-Flattened-Program-definitions!
   o (map (lambda (def)
            (let ((flatfun (make-With-Temp-Function-Definition
                            (Function-Definition-variables def)
                            (Function-Definition-body def)
                            (Function-Definition-free def)
                            (Function-Definition-index def)
                            '() )))
              (collect-temporaries! flatfun flatfun '()) ) )
          (Flattened-Program-definitions o) ) )
  o )

;;; Collect all temporary and local variables that may be needed for
;;; the C evaluation of O. Store these variables in FLATFUN. When they
;;; are identified, they are renamed to avoid name clashes in C and
;;; stored in the R environment.

(define-generic (collect-temporaries! (o Program) flatfun r)
  (update-walk! collect-temporaries! o flatfun r) )

(define-method (collect-temporaries! (o Local-Reference) flatfun r)
  (let* ((variable (Local-Reference-variable o))
         (v (assq variable r)) )
    (if (pair? v) (make-Local-Reference (cdr v)) o) ) )

(define-method (collect-temporaries! (o Box-Creation) flatfun r)
  (let* ((variable (Box-Creation-variable o))
         (v (assq variable r)) )
    (if (pair? v) (make-Box-Creation (cdr v)) o) ) )

;;; For now only variables in Let are considered as temporaries.

(define-method (collect-temporaries! (o Fix-Let) flatfun r)
  (set-Fix-Let-arguments!
   o (collect-temporaries! (Fix-Let-arguments o) flatfun r) )
  (let* ((newvars (map new-renamed-variable                        
                       (Fix-Let-variables o) ))
         (newr (append (map cons (Fix-Let-variables o) newvars) r)) )
    (adjoin-temporary-variables! flatfun newvars)
    (set-Fix-Let-variables! o newvars)
    (set-Fix-Let-body! 
     o (collect-temporaries! (Fix-Let-body o) flatfun newr) )
    o ) )

(define (adjoin-temporary-variables! flatfun newvars)
  (let adjoin ((temps (With-Temp-Function-Definition-temporaries 
                       flatfun ))
               (vars newvars) )
    (if (pair? vars)
        (if (memq (car vars) temps)
            (adjoin temps (cdr vars))
            (adjoin (cons (car vars) temps) (cdr vars)) )
        (set-With-Temp-Function-Definition-temporaries! 
         flatfun temps ) ) ) )

(define renaming-variables-counter 0)

(define-generic (new-renamed-variable (variable)))

(define-method (new-renamed-variable (variable Local-Variable))
  (set! renaming-variables-counter (+ renaming-variables-counter 1))
  (make-Renamed-Local-Variable
   (Variable-name variable) renaming-variables-counter ) )

;;; end of chap10g.scm
