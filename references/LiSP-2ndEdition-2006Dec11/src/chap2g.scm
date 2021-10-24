;;; $Id: chap2g.scm,v 4.0 1995/07/10 06:51:05 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Scheme + introduction of let, letrec, label (added to chap1.scm)

(define (evaluate e env)
  (if (atom? e) 
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e) (boolean? e) (vector? e))
             e )
            (else (wrong "Cannot evaluate" e)) )
      (case (car e)
        ((quote)  (cadr e))
        ((if)     (if (evaluate (cadr e) env)
                      (evaluate (caddr e) env)
                      (evaluate (cadddr e) env) ))
        ((begin)  (eprogn (cdr e) env))
        ((set!)   (update! (cadr e) env (evaluate (caddr e) env)))
        ((lambda) (make-function (cadr e) (cddr e) env))
        ((label)
         (let* ((name    (cadr e))
                (new-env (extend env (list name) (list 'void)))
                (def     (caddr e))
                (fun     (make-function (cadr def) (cddr def) new-env)) )
           (update! name new-env fun)
           fun ) )
        ((let)
         (eprogn (cddr e)
                 (extend env
                         (map (lambda (binding)
                                (if (symbol? binding) binding
                                    (car binding) ) )
                              (cadr e) )
                         (map (lambda (binding)
                                (if (symbol? binding) 
                                    the-non-initialized-marker
                                    (evaluate (cadr binding) env) ) )
                              (cadr e) ) ) ) )
        ((letrec)
         (let ((new-env (extend env 
                                (map car (cadr e))
                                (map (lambda (binding) the-non-initialized-marker)
                                     (cadr e) ) )))
           (map (lambda (binding)       ;\relax {\tt map} to preserve chaos~!
                  (update! (car binding)
                           new-env
                           (evaluate (cadr binding) new-env) ) )
                (cadr e) )
           (eprogn (cddr e) new-env) ) )
        (else     (invoke (evaluate (car e) env)
                          (evlis (cdr e) env) )) ) ) )


(define the-non-initialized-marker (cons 'non 'initialized)) 

(define (lookup id env)
  (if (pair? env)
      (if (eq? (caar env) id)
          (let ((value (cdar env)))
            (if (eq? value the-non-initialized-marker)
                (wrong "Uninitialized binding" id)
                value ) )
          (lookup id (cdr env)) )
      (wrong "No such binding" id) ) )


;;; patch chap1.scm
(set! the-false-value #f)
(definitial f the-false-value)


;;; end of chap2g.scm
