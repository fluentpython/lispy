;;; $Id: chap7e.scm,v 4.1 2006/11/27 11:30:03 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Additions to chap7d.scm
;;; Add dynamic variable, error handler, escape a la bind-exit.

;;; Additional registers

(define *dynenv* -1)
(define *errenv* 1)

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Preserve the state of the machine ie the two environments.

(define (preserve-environment)
  (stack-push *dynenv*)
  (stack-push *env*) )

(define (restore-environment)
  (set! *env* (stack-pop))
  (set! *dynenv* (stack-pop)) )

;;; Preserve the state of the machine ie the three environments.

(define (preserve-environment)
  (stack-push *dynenv*)
  (stack-push *errenv*)
  (stack-push *env*) )

(define (restore-environment)
  (set! *env* (stack-pop))
  (set! *errenv* (stack-pop))
  (set! *dynenv* (stack-pop)) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define-class escape Object
  ( stack-index ) )

(define-method (invoke (f escape) tail?)
  (if (= (+ 1 1) (activation-frame-argument-length *val*))
      (if (escape-valid? f)
          (begin (set! *stack-index* (escape-stack-index f))
                 (set! *val* (activation-frame-argument *val* 0))
                 (set! *pc* (stack-pop)) )
          (signal-exception #f (list "Escape out of extent" f)) )
      (signal-exception #f (list "Incorrect arity" 'escape)) ) )

(define escape-tag (list '*ESCAPE*))

(define (escape-valid? f)
  (let ((index (escape-stack-index f)))
    (and (>= *stack-index* index)
         (eq? f (vector-ref *stack* (- index 3)))
         (eq? escape-tag (vector-ref *stack* (- index 2))) ) ) )

(define-method (show (e escape) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (format stream "#<Escape valid?=~A, stack-index=~A>"
            (escape-valid? e) (escape-stack-index e) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; add bind-exit, dynamic and dynamic-let, monitor as new special forms.

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
        (else     (meaning-application (car e) (cdr e) r tail?)) ) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooo bind-exit

(define (meaning-bind-exit n e+ r tail?)
  (let* ((r2 (r-extend* r (list n)))
         (m+ (meaning-sequence e+ r2 #t)) )
    (ESCAPER m+) ) )

(define (ESCAPER m+)
  (append (PUSH-ESCAPER (+ 1 (length m+))) 
          m+ (RETURN) (POP-ESCAPER) ) )

(define (POP-ESCAPER) (list 250))

(define (PUSH-ESCAPER offset) (list 251 offset))

;;;ooooooooooooooooooooooooooooooooooooooooooo Dynamic variables

(define (meaning-dynamic-let n e e+ r tail?)
  (let ((index (get-dynamic-variable-index n))
        (m (meaning e r #f))
        (m+ (meaning-sequence e+ r #f)) )
    (append m (DYNAMIC-PUSH index) m+ (DYNAMIC-POP)) ) )

(define (meaning-dynamic-reference n r tail?)
  (let ((index (get-dynamic-variable-index n)))
    (DYNAMIC-REF index) ) )

(define (DYNAMIC-POP) (list 241))

(define (DYNAMIC-PUSH index) (list 242 index))

(define (DYNAMIC-REF index) (list 240 index))

;;; Dynamic variables do not use index 0 (which will be used by error
;;; handlers).

(define (get-dynamic-variable-index n)
  (let ((where (memq n *dynamic-variables*)))
    (if where (length where)
        (begin
          (set! *dynamic-variables* (cons n *dynamic-variables*))
          (length *dynamic-variables*) ) ) ) )

(define *dynamic-variables* '())

;;; Search a value in the stack.

(define (find-dynamic-value index)
  (let search ((i (search-dynenv-index)))
    (if (>= i 0)
        (if (= index (vector-ref *stack* i))
            (vector-ref *stack* (- i 1))
            (search (vector-ref *stack* (- i 2))) )
        (signal-exception #t (list "No such dynamic binding" index)) ) ) )

(define (pop-dynamic-binding)
  (stack-pop)
  (stack-pop)
  (set! *dynenv* (stack-pop)) )

(define (push-dynamic-binding index value)
  (stack-push *dynenv*)
  (stack-push value)
  (stack-push index)
  (set! *dynenv* (- *stack-index* 1)) )

(define (search-dynenv-index)
  *dynenv* )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Errors.
;;; The initial code segment starts with 
;;;    (NON-CONT-ERR) (FINISH) (RESTORE-ENV) (RETURN) start-pc
;;; The initial stack contains the initial error handler
;;;     stack[0] = 1                  (next error handler (the same))
;;;     stack[1] = base-error-handler
;;; as well as the return address of FINISH (pc = 1).

(define (code-prologue)
  (set! finish-pc 1)
  (append (NON-CONT-ERR) (FINISH) (RESTORE-ENV) (RETURN)) )

(define (signal-exception continuable? anomaly)
  (let* ((index *errenv*)
         (handler (vector-ref *stack* index))
         (v* (allocate-activation-frame (+ 2 1))) )
    (set-activation-frame-argument! v* 0 continuable?)
    (set-activation-frame-argument! v* 1 anomaly)
    (set! *val* v*)
    (cond (continuable?
           (stack-push *pc*)
           (preserve-environment)
           (stack-push 2) )             ; pc for (RESTORE-ENV) (RETURN)
          (else (stack-push 0))  )      ; pc for (NON-CONT-ERR)
    (set! *errenv* (vector-ref *stack* (- index 1)))
    (invoke handler #t) ) )
 
(define (base-error-handler)
  (show-registers "Panic error: content of registers:")
  (wrong "Abort") )

(define (meaning-monitor e e+ r tail?)
  (let ((m (meaning e r #f))
        (m+ (meaning-sequence e+ r #f)) )
    (append m (PUSH-HANDLER) m+ (POP-HANDLER)) ) )

(define (NON-CONT-ERR) (list 245))

(define (PUSH-HANDLER) (list 246))

(define (POP-HANDLER)  (list 247))

(define (pop-exception-handler)
  (stack-pop)
  (set! *errenv* (stack-pop)) )

(define (push-exception-handler)
  (stack-push *errenv*)
  (set! *errenv* *stack-index*)
  (stack-push *val*) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define (show-registers message)
  (when *debug* 
    (format #t "~%----------------~A" message)
    (format #t "~%ENV  = ") (show *env*)
    (format #t "~%DYN  = ") (show *dynenv*)
    (format #t "~%ERR  = ") (show *errenv*)
    (format #t "~%VAL  = ") (show *val*)
    (format #t "~%FUN  = ") (show *fun*)
    (show-stack (save-stack))
    (format #t "~%(PC  = ~A), next INSTR to be executed = ~A~%" 
            *pc* (instruction-decode *code* *pc*) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define (chapter7e-interpreter)
  (define (toplevel)
    (let* ((e (read))
           (code (make-code-segment (meaning e r.init #t)))
           (global-names (map car (reverse g.current)))
           (constants (apply vector *quotations*))
           (dynamics *dynamic-variables*)
           (start-pc (length (code-prologue))) )
      (run-machine start-pc code constants global-names dynamics)
      (display *val*)
      (toplevel) ) )
  (toplevel) ) 

(define (stand-alone-producer7e e)
  (set! g.current (original.g.current))
  (set! *quotations* '())
  (set! *dynamic-variables* '())
  (let* ((code (make-code-segment (meaning e r.init #t)))
         (global-names (map car (reverse g.current)))
         (constants (apply vector *quotations*))
         (dynamics *dynamic-variables*)
         (start-pc (length (code-prologue))) )
    (lambda ()
      (run-machine start-pc code constants global-names dynamics) ) ) )

(define (run-machine pc code constants global-names dynamics)
  (define base-error-handler-primitive
    (make-primitive base-error-handler) )
  (set! sg.current (make-vector (length global-names) undefined-value))
  (set! sg.current.names    global-names)
  (set! *constants*         constants)
  (set! *dynamic-variables* dynamics)
  (set! *code*              code)
  (set! *env*               sr.init)
  (set! *dynenv*            -1)         ; empty dynamic environment
  (set! *errenv*            1)
  (set! *stack-index*       0)
  (set! *val*               'anything)
  (set! *fun*               'anything)
  (set! *arg1*              'anything)
  (set! *arg2*              'anything)
  (stack-push 1)                        ; push the base error handler
  (stack-push base-error-handler-primitive)
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

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Tests

(define (scheme7e)
  (interpreter
   "Scheme? "  
   "Scheme= " 
   #t
   (lambda (read print error)
     (setup-wrong-functions error)
     (lambda ()
       ((stand-alone-producer7e (read)))
       (print *val*) ) ) ) )

(define (test-scheme7e file)
  (suite-test 
   file 
   "Scheme? " 
   "Scheme= "
   #t
   (lambda (read check error)
     (setup-wrong-functions error)
     (lambda ()
       ((stand-alone-producer7e (read)))
       (check *val*) ) )
   equal? ) )

(define (setup-wrong-functions error)
  (set! wrong (lambda args
                (format #t "
		>>>>>>>>>>>>>>>>>>RunTime PANIC<<<<<<<<<<<<<<<<<<<<<<<<<
		~A~%" (activation-frame-argument *val* 1) )
                (apply error args) ))
  (set! static-wrong (lambda args
                       (format #t "
		>>>>>>>>>>>>>>>>>>Static WARNING<<<<<<<<<<<<<<<<<<<<<<<<<
		~A~%" args )
                       (apply error args) )) )

;;; end of chap7e.scm
