;;; $Id: chap8d.scm,v 4.1 2006/11/24 18:41:05 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; variant of chap8c.scm for the bytecode compiler chap7g.scm

(define (meaning e r tail?)
  (if (atom? e)
      (if (symbol? e) (meaning-reference e r tail?)
                      (meaning-quotation e r tail?) )
      (case (car e)
        ((quote)  (meaning-quotation (cadr e) r tail?))
        ((lambda) (meaning-abstraction (cadr e) (cddr e) r tail?))
        ((if)     (meaning-alternative (cadr e) (caddr e) (cadddr e) r tail?))
        ((begin)  (meaning-sequence (cdr e) r tail?))
        ((set!)   (meaning-assignment (cadr e) (caddr e) r tail?))
        ((bind-exit) (meaning-bind-exit (caadr e) (cddr e) r tail?))
        ((dynamic) (meaning-dynamic-reference (cadr e) r tail?))
        ((dynamic-let) (meaning-dynamic-let (car (cadr e))
                                            (cadr (cadr e))
                                            (cddr e) r tail? ))
        ((monitor) (meaning-monitor (cadr e) (cddr e) r tail?))
        ((eval)   (meaning-eval (cadr e) r tail?))
        (else     (meaning-application (car e) (cdr e) r tail?)) ) ) )

(define (meaning-eval e r tail?)
  (let ((m (meaning e r #f)))
    (EVAL/CE m r) ) )

(define (EVAL/CE m r)
  (append (PRESERVE-ENV) (CONSTANT r) (PUSH-VALUE) 
          m (COMPILE-RUN) (RESTORE-ENV) ) )

(define (COMPILE-RUN) (list 255))

(define (compile-and-run v r tail?)
  (unless tail? (stack-push *pc*))
  (set! *pc* (compile-on-the-fly v r)) )

;;; Compile program v within environment r, install resulting code and
;;; return its entry point.

(define (compile-on-the-fly v r)
  (set! g.current '())
  (for-each g.current-extend! sg.current.names)
  (set! *quotations* (vector->list *constants*))
  (set! *dynamic-variables* *dynamic-variables*)
  (let ((code (apply vector (append (meaning v r #f) (RETURN)))))
    (set! sg.current.names (map car (reverse g.current)))
    (let ((v (make-vector (length sg.current.names) 
                          undefined-value )))
      (vector-copy! sg.current v 0 (vector-length sg.current))
      (set! sg.current v) )
    (set! *constants* (apply vector *quotations*))
    (set! *dynamic-variables* *dynamic-variables*)
    (install-code! code) ) )

;;; Exercice: Share the compilation.

(define (prepare e)
  (eval/ce `(lambda () ,e)) )

;;; Exercice: eval/at with eval/ce without clash.

(define (eval/at e)
  (let ((g (gensym)))
    (eval/ce `(lambda (,g) (eval/ce ,g))) ) )

;;; end of chap8d.scm
