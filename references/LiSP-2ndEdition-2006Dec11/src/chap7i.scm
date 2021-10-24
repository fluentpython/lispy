;;; $Id: chap7i.scm,v 4.1 1996/01/14 14:14:29 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Dynamic variables with shallow binding
;;; Just redefine find-dynamic-value, pop-dynamic-binding and
;;; push-dynamic-binding.

(define (find-dynamic-value index)
  (let ((v (vector-ref *dynamics* index)))
    (if (eq? v undefined-value)
        (signal-exception #f (list "No such dynamic binding" index))
        v ) ) )

(define (push-dynamic-binding index value)
  (stack-push (vector-ref *dynamics* index))
  (stack-push index)
  (vector-set! *dynamics* index value) )

(define (pop-dynamic-binding)
  (let* ((index (stack-pop))
         (old-value (stack-pop)) )
    (vector-set! *dynamics* index old-value) ) )

(define *dynamics* (vector 1))

(define (run-machine pc code constants global-names dynamics)
  (define base-error-handler-primitive
    (make-primitive base-error-handler) )
  (set! sg.current (make-vector (length global-names) undefined-value))
  (set! sg.current.names    global-names)
  (set! *constants*         constants)
  (set! *dynamic-variables* dynamics)
  (set! *dynamics* (make-vector (+ 1 (length dynamics))
                                undefined-value )) ; \modified
  (set! *code*              code)
  (set! *env*               sr.init)
  (set! *stack-index*       0)
  (set! *val*               'anything)
  (set! *fun*               'anything)
  (set! *arg1*              'anything)
  (set! *arg2*              'anything)
  (push-dynamic-binding 0 (list base-error-handler-primitive))
  (stack-push finish-pc)                ;  pc for FINISH
  (set! *pc*                pc)
  (call/cc (lambda (exit)
             (set! *exit* exit)
             (run) )) )

(let ((native-run-machine run-machine))
  (set! run-machine
        (lambda (pc code constants global-names dynamics)
          (when *debug*                     ; DEBUG
            (format #t "Code= ~A~%" (disassemble code)) )         
          (native-run-machine pc code constants global-names dynamics) ) ) )

;;; end of chap7i.scm
