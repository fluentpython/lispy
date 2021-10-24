;;; $Id: chap9c.scm,v 4.6 2006/11/27 11:46:03 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; A low-level macro system offering hygien.

;;; Variables are not Programs! They represent bindings.

(define-class Variable Object (name))

(define-class Global-Variable Variable ())

(define-class Predefined-Variable Variable (description))

(define-class Local-Variable Variable (mutable? dotted?))

;;; Descriptions

(define-class Functional-Description Object (comparator arity generator))

(define-class Constant-Description Object (value))

;;; Special category of keywords

(define-class Magic-Keyword Object (name handler))

;;; Programs

(define-class Program Object ())

;;; References are programs.

(define-class Reference Program (variable))

(define-class Local-Reference Reference ())

(define-class Global-Reference Reference ())

(define-class Predefined-Reference Reference ())

(define-class Local-Assignment Program (reference form))

(define-class Global-Assignment Program (variable form))

(define-class Function Program (variables body))

(define-class Alternative Program (condition consequent alternant))

(define-class Sequence Program (first last))

(define-class Constant Program (value) )

(define-class Application Program ())

(define-class Regular-Application Application (function arguments))

(define-class Predefined-Application Application 
  (variable arguments) )

(define-class Fix-Let Program (variables arguments body))

(define-class Arguments Program (first others))

(define-class No-Argument Program ())

;;;

(define-class Environment Object (next))

(define-class Full-Environment Environment (variable))

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; No syntactic checks here.

(define (objectify e r)
  (if (atom? e)
      (cond ((Magic-Keyword? e) e)
            ((Program? e)       e)
            ((symbol? e)        (objectify-symbol e r))
            (else               (objectify-quotation e r)) )
      (let ((m (objectify (car e) r)))
        (if (Magic-Keyword? m)
            ((Magic-Keyword-handler m) e r)
            (objectify-application m (cdr e) r)) ) ) )

(define (objectify-quotation value r)
  (make-Constant value) )

(define (objectify-alternative ec et ef r)
  (make-Alternative (objectify ec r)
                    (objectify et r)
                    (objectify ef r) ) )

(define (objectify-sequence e* r)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (let ((a (objectify (car e*) r)))
            (make-Sequence a (objectify-sequence (cdr e*) r)) )
          (objectify (car e*) r) )
      (make-Constant 42) ) )

(define (objectify-application ff e* r)
  (let ((ee* (convert2arguments (map (lambda (e) (objectify e r)) 
                                     e* ))) )
    (cond ((Function? ff)  
           (process-closed-application ff ee*) )
          ((Predefined-Reference? ff) 
           (let* ((fvf (Predefined-Reference-variable ff))
                  (desc (Predefined-Variable-description fvf)) )
             (if (Functional-Description? desc)
                 (if ((Functional-Description-comparator desc)
                      (length e*) 
                      (Functional-Description-arity desc) )
                     (make-Predefined-Application fvf ee*)
                     (objectify-error 
                      "Incorrect predefined arity" ff e* ) )
                 (make-Regular-Application ff ee*) ) ) )
          (else (make-Regular-Application ff ee*)) ) ) )

(define (process-closed-application f e*)
  (let ((v* (Function-variables f))
        (b  (Function-body f)) )
    (if (and (pair? v*) (Local-Variable-dotted? (car (last-pair v*))))
        (process-nary-closed-application f e*) 
        (if (= (number-of e*) (length (Function-variables f)))
            (make-Fix-Let (Function-variables f) e* (Function-body f))
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
                            (find-variable? 'cons g.predef)
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
                        (objectify-error 
                         "Incorrect dotted arity" f e* ) ) ) )
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
           (set-Local-Variable-mutable?! 
            (Local-Reference-variable ov) #t )
           (make-Local-Assignment ov of) )
          ((Global-Reference? ov)
           (make-Global-Assignment (Global-Reference-variable ov) 
                                   of ) )
          (else (objectify-error 
                 "Illegal mutated reference" variable )) ) ) )

;;; It is important that the body is objectified first, so mutability
;;; of local variables is known and they can be appropriately handled
;;; in objectify-variables-list. 
;;; All variables are considered immutable at the beginning.

(define (objectify-function names body r)
  (let* ((vars (objectify-variables-list names))
         (b    (objectify-sequence body (r-extend* r vars))) )
    (make-Function vars b) ) )

(define (objectify-variables-list names)
  (if (pair? names)
      (cons (make-Local-Variable (car names) #f #f)
            (objectify-variables-list (cdr names)) )
      (if (symbol? names)
          (list (make-Local-Variable names #f #t))
          '() ) ) )

(define (objectify-symbol variable r)
  (let ((v (find-variable? variable r)))
    (cond ((Magic-Keyword? v)       v)
          ((Local-Variable? v)      (make-Local-Reference v))
          ((Global-Variable? v)     (make-Global-Reference v))
          ((Predefined-Variable? v) (make-Predefined-Reference v))
          (else (objectify-free-global-reference variable r)) ) ) )

;(define (objectify-free-global-reference variable r)
;  (objectify-error "No such variable" variable) )

;;; A free variable is defined to be a global variable defined on the fly.

(define (objectify-free-global-reference name r)
  (let ((v (make-Global-Variable name)))
    (insert-global! v r)
    (make-Global-Reference v) ) )

;;; These functions deal with the environment.  R is a sequence of
;;; Full-Environment objects containing local variables followed by an
;;; instance of Environment followed by a sequence of Full-Environment
;;; containing the global mutable bindings mixed with the global
;;; macros followed by the predefined bindings.

(define (r-extend* r vars)
  (if (pair? vars)
      (r-extend (r-extend* r (cdr vars)) (car vars))
      r ) )

(define (r-extend r var)
  (make-Full-Environment r var) )

(define (insert-global! variable r)
  (let ((r (find-global-environment r)))
    (set-Environment-next! 
     r (make-Full-Environment (Environment-next r) variable) ) ) )

(define (find-global-environment r)
  (if (Full-Environment? r) 
      (find-global-environment (Full-Environment-next r))
      r ) )

(define (find-variable? name r)
  (if (Full-Environment? r)
      (let ((var (Full-Environment-variable r)))
        (if (eq? name 
                 (cond ((Variable? var) (Variable-name var))
                       ((Magic-Keyword? var) 
                        (Magic-Keyword-name var) ) ) )
            var
            (find-variable? name (Full-Environment-next r)) ) )
      (if (Environment? r)
          (find-variable? name (Environment-next r))
          #f ) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Creating the environment defining special form walkers.

(define special-if 
  (make-Magic-Keyword
   'if (lambda (e r)
         (objectify-alternative (cadr e) (caddr e) (cadddr e) 
                                r ) ) ) )

(define special-begin
  (make-Magic-Keyword
   'begin (lambda (e r) 
            (objectify-sequence (cdr e) r) ) ) )

(define special-quote
  (make-Magic-Keyword 
   'quote (lambda (e r)
            (objectify-quotation (cadr e) r) ) ) )

(define special-set!
  (make-Magic-Keyword
   'set! (lambda (e r)
           (objectify-assignment (cadr e) (caddr e) r) ) ) )

(define special-lambda
  (make-Magic-Keyword
   'lambda (lambda (e r)
             (objectify-function (cadr e) (cddr e) r) ) ) )

(define special-let
  (make-Magic-Keyword
   'let (lambda (e r)
          (if (symbol? (cadr e))
              ;; (let name ((var val)...) form...)   Q&D
              (objectify 
               `((,special-let ((,(cadr e) #f))
                   (,special-set! ,(cadr e) 
                                  (,special-lambda ,(map car (caddr e))
                                                   . ,(cdddr e) ) )
                  ,(cadr e) )
                 . ,(map cadr (caddr e)) )
               r )
              ;; (let ((var val)...) form...)
              (objectify-application
               (objectify-function (map car (cadr e))
                                   (cddr e)
                                   r )
               (map cadr (cadr e))
               r ) ) ) ) )

(define *special-form-keywords* 
  (list special-quote
        special-if
        special-begin
        special-set!
        special-lambda
        ;; cond, letrec, etc.
        special-let
        ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Other macros seen from every level.

(define special-when
  (make-Magic-Keyword
   'when (lambda (e r)
           (let ((condition (cadr e))
                 (body      (cddr e)) )
             (objectify `(,special-if ,condition
                                      (,special-begin . ,body)
                                      #f )
                        r ) ) ) ) )

;;; Backquote forms are supposed to be correct.  This is very ugly and
;;; only approximate. Backquoting should be better interleaved with
;;; objectification. What if unquote shadows lexically a comma ?
;;; QUICK and DIRTY!

(define special-quasiquote
  (make-Magic-Keyword
   'quasiquote
   (lambda (e r)
     (define (walk e)
       (if (pair? e)
           (if (eq? (car e) 'unquote)
               (cadr e)
               (if (eq? (car e) 'quasiquote)
                   (objectify-error "No embedded quasiquotation" e)
                   (walk-pair e) ) )
           (list special-quote e) ) )
     (define (walk-pair e)
       (if (pair? (car e))
           (if (eq? (car (car e)) 'unquote-splicing)
               (list (make-Predefined-Reference 
                      (find-variable? 'append g.predef) )
                     (cadr (car e))
                     (walk (cdr e)) )
               (list (make-Predefined-Reference
                      (find-variable? 'cons g.predef) )
                     (walk (car e))
                     (walk (cdr e)) ) )
           (list (make-Predefined-Reference
                  (find-variable? 'cons g.predef) )
                 (list special-quote (car e))
                 (walk (cdr e)) ) ) )
     (objectify (walk (cadr e)) r) ) ) )

(set! *special-form-keywords* 
      (append (list special-when
                    special-quasiquote )
              *special-form-keywords* ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; src/chap9d.scm creates g.predef (and sg.predef) with predefined bindings.
;;; To this environment, one must add special keywords, predefined macros etc.
;;; but add them only to g since they are useless to sg.

(define-class Evaluator Object 
  ( mother
    Preparation-Environment
    RunTime-Environment
    eval
    expand 
    ) )

;;; A fake copy for the book less the debug information

(define (create-evaluator old-level)
  (let ((level 'wait)
        (g     g.predef)
        (sg    sg.predef) )
    (define (expand e)
      (let ((prg (objectify 
                  e (Evaluator-Preparation-Environment level) )))
        (enrich-with-new-global-variables! level)
        prg ) )
    (define (eval e)
      (let ((prg (expand e)))
        (evaluate prg (Evaluator-RunTime-Environment level)) ) )
    ;; Create resulting evaluator instance
    (set! level (make-Evaluator old-level 'wait 'wait eval expand))
    ;; Enrich environment with {\tt eval}
    (set! g (r-extend* g *special-form-keywords*))
    (set! g (r-extend* g (make-macro-environment level)))
    (let ((eval-var (make-Predefined-Variable 
                     'eval (make-Functional-Description = 1 "") ))
          (eval-fn (make-RunTime-Primitive eval = 1)) )
      (set! g (r-extend g eval-var))
      (set! sg (sr-extend sg eval-var eval-fn)) )
    ;; Mark the beginning of the global environment
    (set-Evaluator-Preparation-Environment! 
     level (mark-global-preparation-environment g) )
    (set-Evaluator-RunTime-Environment! 
     level (mark-global-runtime-environment sg) )
    level ) )

(define (create-evaluator old-level)
  (let ((level 'wait)
        (g     g.predef)
        (sg    sg.predef) )
    (define (expand e)
      (let ((prg (objectify e (Evaluator-Preparation-Environment level))))
        (enrich-with-new-global-variables! level)
        (show-expansion e prg)          ; DEBUG
        prg ) )
    (define (eval e)
      (let ((prg (expand e)))
        (evaluate prg (Evaluator-RunTime-Environment level)) ) )
    ;; Create resulting evaluator instance
    (set! level (make-Evaluator old-level 'wait 'wait eval expand))
    ;; Enrich environment with {\tt eval}
    (set! g (r-extend* g *special-form-keywords*))
    (set! g (r-extend* g (make-macro-environment level)))
    (let ((eval-var (make-Predefined-Variable 
                     'eval (make-Functional-Description = 1 "") ))
          (eval-fn (make-RunTime-Primitive eval = 1)) )
      (set! g (r-extend g eval-var))
      (set! sg (sr-extend sg eval-var eval-fn)) )
    ;; Mark the beginning of the global environment
    (set-Evaluator-Preparation-Environment! 
     level (mark-global-preparation-environment g) )
    (set-Evaluator-RunTime-Environment! 
     level (mark-global-runtime-environment sg) )
    level ) )

(define (mark-global-preparation-environment g)
  (make-Environment g) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; for debug purposes, show expansion at work

(define (show-expansion e prg)
  (format #t "  expression: ")
  (show e)
  (newline)
  (format #t "  gave: ")
  (show (->Scheme prg '()))
  (newline) )

;;; For debug purposes, show active macros

(define (show-active-macros level)
  (let ((macros (let scan ((r (Evaluator-Preparation-Environment level)))
                  (if (Full-Environment? r)
                      (if (Magic-Keyword? (Full-Environment-variable r))
                          (cons (magic-Keyword-name 
                                 (Full-Environment-variable r) )
                                (scan (Environment-next r)) )
                          (scan (Environment-next r)) )
                      (if (Environment? r)
                          (scan (Environment-next r))
                          '() ) ) )))
    (format #t "Active macros are: ~A~%" macros) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Creating the environment of predefined macros. They will be seen
;;; from every level of evaluators. 

(define (special-eval-in-abbreviation-world level)
  (lambda (e r)
    (let ((body (cdr e)))
      (objectify ((Evaluator-eval (force level)) 
                  `(,special-begin . ,body) )
                 r ) ) ) )

;;; The sole syntax is
;;; (define-abbreviation (foo . <parameters>) <body>)

(define (special-define-abbreviation level)
  (lambda (e r)
    (let* ((call      (cadr e))
           (body      (cddr e))
           (name      (car call))
           (variables (cdr call)) )
      (let ((expander ((Evaluator-eval (force level))
                       `(,special-lambda ,variables . ,body) )))
        (define (handler e r)
          (objectify (invoke expander (cdr e)) r) )
        (insert-global! (make-Magic-Keyword name handler) r)
        (objectify #t r) ) ) ) )

;;; Actually the syntax of let-abbreviation resembles that of define-syntax
;;; and thus there is a lot of parentheses to specify local macros.
;;; ie (let-abbreviation ( ( (foo . parms) body )
;;;                        <other-macros> )
;;;       <body> )

(define (special-let-abbreviation level)
  (lambda (e r)
    (let ((level  (force level))
          (macros (cadr e))
          (body   (cddr e)) )
      (define (make-macro def)
        (let* ((call      (car def))
               (body      (cdr def))
               (name      (car call))
               (variables (cdr call)) )
          (let ((expander ((Evaluator-eval level)
                           `(,special-lambda ,variables . ,body) )))
            (define (handler e r)
              (objectify (invoke expander (cdr e)) r) )
            (make-Magic-Keyword name handler) ) ) )
      (objectify `(,special-begin . ,body)
                 (r-extend* r (map make-macro macros)) ) ) ) )

;;; Syntax is (with-aliases ( (<variable> <word>) ... )
;;;               <body> )
;;; Like a let in the macroworld space but where bound values refer to the
;;; current compilation. Also affects the macro-eval process.

(define (special-with-aliases level)
  (lambda (e current-r)
    (let* ((level   (force level))
           (oldr    (Evaluator-Preparation-Environment level))
           (oldsr   (Evaluator-RunTime-Environment level))
           (aliases (cadr e))
           (body    (cddr e)) )
      (let bind ((aliases aliases)
                 (r       oldr)
                 (sr      oldsr) )
        (if (pair? aliases)
            (let* ((variable (car (car aliases)))
                   (word     (cadr (car aliases)))
                   (var      (make-Local-Variable variable #f #f)) )
              (bind (cdr aliases)
                    (r-extend r var) 
                    (sr-extend sr var (objectify word current-r)) ) )
            (let ((result 'wait))
              (set-Evaluator-Preparation-Environment! level r)
              (set-Evaluator-RunTime-Environment! level sr)
              (set! result (objectify `(,special-begin . ,body) 
                                      current-r ))
              (set-Evaluator-Preparation-Environment! level oldr)
              (set-Evaluator-RunTime-Environment! level oldsr)
              result ) ) ) ) ) )

;;; si on utilise des effets de bords pb avec continuation dans macroworld
;;; unwind-protect serait mieux.

(define (make-macro-environment current-level)
  (let ((metalevel (delay (create-evaluator current-level))))
    (list (make-Magic-Keyword 'eval-in-abbreviation-world 
           (special-eval-in-abbreviation-world metalevel) )
          (make-Magic-Keyword 'define-abbreviation 
           (special-define-abbreviation metalevel))
          (make-Magic-Keyword 'let-abbreviation    
           (special-let-abbreviation metalevel))
          (make-Magic-Keyword 'with-aliases   
           (special-with-aliases metalevel) ) ) ) )

;;; Additional global variables

(define objectify-error 'wait)
(define evaluate-error 'wait)

;;; end of chap9c.scm
