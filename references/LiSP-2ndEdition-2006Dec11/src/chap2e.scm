;;; $Id: chap2e.scm,v 4.2 2006/11/09 18:58:18 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Variants around chap2c.scm a Common-Lisp approximation for
;;; variable handling.

(define (df.evaluate e env fenv denv)
  (if (atom? e) 
      (cond ((symbol? e) (cl.lookup e env denv))
            ((or (number? e) (string? e) (char? e)
                 (boolean? e) (vector? e) )
             e )
            (else (wrong "Cannot evaluate" e)) )
      (case (car e)
        ((quote) (cadr e))
        ((if) (if (df.evaluate (cadr e) env fenv denv)
                  (df.evaluate (caddr e) env fenv denv)
                  (df.evaluate (cadddr e) env fenv denv) ))
        ((begin) (df.eprogn (cdr e) env fenv denv))
        ((set!) (cl.update! (cadr e) 
                            env 
                            denv
                            (df.evaluate (caddr e) env fenv denv) ))
        ((function)
         (cond ((symbol? (cadr e))
                (f.lookup (cadr e) fenv) )
               ((and (pair? (cadr e)) (eq? (car (cadr e)) 'lambda))
                (df.make-function 
                 (cadr (cadr e)) (cddr (cadr e)) env fenv ) )
               (else (wrong "Incorrect function" (cadr e))) ) )
        ((dynamic) (lookup (cadr e) denv))
        ((dynamic-let)
         (df.eprogn (cddr e)
                    (special-extend env             ;\modified
                                    (map car (cadr e)) )
                    fenv
                    (extend denv 
                            (map car (cadr e))
                            (map (lambda (e) 
                                   (df.evaluate e env fenv denv) )
                                 (map cadr (cadr e)) ) ) ) )
        (else (df.evaluate-application 
               (car e) 
               (df.evlis (cdr e) env fenv denv)
               env 
               fenv
               denv )) ) ) )

(define (special-extend env variables)
  (append variables env) )

(define (cl.lookup var env denv)
  (let look ((env env))
    (if (pair? env)
        (if (pair? (car env))
            (if (eq? (caar env) var)
                (cdar env)
                (look (cdr env)) )
            (if (eq? (car env) var)
                ;; lookup in the current dynamic environment
                (let lookup-in-denv ((denv denv))
                  (if (pair? denv)
                      (if (eq? (caar denv) var)
                          (cdar denv)
                          (lookup-in-denv (cdr denv)) )
                      ;; default to the global lexical environment
                      (lookup var env.global) ) )
                (look (cdr env)) ) )
        (wrong "No such binding" var) ) ) )

(define (cl.update! var env denv value)
  (let look ((env env))
    (if (pair? env)
        (if (pair? (car env))
            (if (eq? (caar env) var)
                (set-cdr! (car env) value)
                (look (cdr env)) )
            (if (eq? (car env) var)
                ;; lookup in the current dynamic environment
                (let lookup-in-denv ((denv denv))
                  (if (pair? denv)
                      (if (eq? (caar denv) var)
                          (set-cdr! (car denv) value)
                          (lookup-in-denv (cdr denv)) )
                      ;; default to the global lexical environment
                      (lookup var env.global) ) )
                (look (cdr env)) ) )
        (wrong "No such binding" var) ) ) )

;;; end of chap2e.scm
