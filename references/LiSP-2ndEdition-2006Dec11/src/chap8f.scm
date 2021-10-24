;;; $Id: chap8f.scm,v 4.1 2006/11/24 18:40:55 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; eval as a function in the bytecode compiler

(definitial eval
  (let* ((arity 1)
         (arity+1 (+ arity 1)) )
    (make-primitive
     (lambda ()
       (if (= arity+1 (activation-frame-argument-length *val*))
           (let ((v (activation-frame-argument *val* 0)))
             (if (program? v)
                 (compile-and-run v r.init #t)
                 (signal-exception #t (list "Illegal program" v)) ) )
           (signal-exception 
            #t (list "Incorrect arity" 'eval) ) ) ) ) ) )

;;; same as in chap8d.scm 

(define (compile-and-run v r tail?)
  (set! g.current '())
  (for-each g.current-extend! sg.current.names)
  (set! *quotations* (vector->list *constants*))
  (set! *dynamic-variables* *dynamic-variables*)
  (let ((code (apply vector (append (meaning v r #f) (RETURN)))))
    (set! sg.current.names (map car (reverse g.current)))
    (let ((v (make-vector (length sg.current.names) undefined-value)))
      (vector-copy! sg.current v 0 (vector-length sg.current))
      (set! sg.current v) )
    (set! *constants* (apply vector *quotations*))
    (set! *dynamic-variables* *dynamic-variables*)
    (unless tail? (stack-push *pc*))
    (set! *pc* (install-code! code)) ) )

;;; end of chap8f.scm
