<;;; $Id: chap7d.scm,v 4.2 2006/11/24 18:16:32 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Refinement of chap6d and chap7c. This interpreter introduces a
;;; *val* register and a *stack* to save/restore arguments that wait
;;; to be stored in an activation block. Functions now take their
;;; activation frame in the *val* register. Code is now a list of
;;; bytes.

;;; Load chap6d before.

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The runtime machine

;(define *env* #f) ; already appears in chap6d
(define *val* #f)
(define *fun* #f)
(define *arg1* #f)
(define *arg2* #f)

(define *pc* 0)
(define *code* (vector 20))

(define *constants* (vector))

;;; Some tests depend on 100 being the depth of the stack.
(define *stack* (make-vector 100))
(define *stack-index* 0)

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define (stack-push v)
  (vector-set! *stack* *stack-index* v)
  (set! *stack-index* (+ *stack-index* 1)) )

(define (stack-pop)
  (set! *stack-index* (- *stack-index* 1))
  (vector-ref *stack* *stack-index*) )

(define (save-stack)
  (let ((copy (make-vector *stack-index*)))
    (vector-copy! *stack* copy 0 *stack-index*)
    copy ) )

(define (restore-stack copy)
  (set! *stack-index* (vector-length copy))
  (vector-copy! copy *stack* 0 *stack-index*) )

;;; Copy vector old[start..end[ into vector new[start..end[
(define (vector-copy! old new start end)
  (let copy ((i start))
    (when (< i end)
          (vector-set! new i (vector-ref old i))
          (copy (+ i 1)) ) ) )

(define (quotation-fetch i)
  (vector-ref *constants* i) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; make them inherit from invokable.

(define-class primitive Object
  ( address ) )

(define-class continuation Object
  ( stack ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; This global variable holds at preparation time all the interesting
;;; quotations. It will be converted into *constants* for run-time.
;;; Quotations are not compressed and can appear multiply.

(define *quotations* (list))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Combinators that just expand into instructions.

(define (SHALLOW-ARGUMENT-SET! j m)
  (append m (SET-SHALLOW-ARGUMENT! j)) )

(define (DEEP-ARGUMENT-SET! i j m)
  (append m (SET-DEEP-ARGUMENT! i j)) )

(define (GLOBAL-SET! i m)
  (append m (SET-GLOBAL! i)) )

;;; GOTO is not necessary if m2 is a tail-call but don't care.
;;; This one changed since chap7c.scm

(define (ALTERNATIVE m1 m2 m3)
  (let ((mm2 (append m2 (GOTO (length m3)))))
    (append m1 (JUMP-FALSE (length mm2)) mm2 m3) ) )

(define (SEQUENCE m m+)
  (append m m+) )

(define (TR-FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+) )

(define (FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+ (UNLINK-ENV)) )

(define (CALL0 address)
  (INVOKE0 address) )

(define (CALL1 address m1)
  (append m1 (INVOKE1 address) ) )

(define (CALL2 address m1 m2)
  (append m1 (PUSH-VALUE) m2 (POP-ARG1) (INVOKE2 address)) )

(define (CALL3 address m1 m2 m3)
  (append m1 (PUSH-VALUE) 
          m2 (PUSH-VALUE) 
          m3 (POP-ARG2) (POP-ARG1) (INVOKE3 address) ) )

(define (FIX-CLOSURE m+ arity)
  (let* ((the-function (append (ARITY=? (+ arity 1)) (EXTEND-ENV)
                               m+  (RETURN) ))
         (the-goto (GOTO (length the-function))) )
    (append (CREATE-CLOSURE (length the-goto)) the-goto the-function) ) )

(define (NARY-CLOSURE m+ arity)
  (let* ((the-function (append (ARITY>=? (+ arity 1)) (PACK-FRAME! arity)
                               (EXTEND-ENV) m+ (RETURN) ))
         (the-goto (GOTO (length the-function))) )
    (append (CREATE-CLOSURE (length the-goto)) the-goto the-function) ) )

(define (TR-REGULAR-CALL m m*)
  (append m (PUSH-VALUE) m* (POP-FUNCTION) (FUNCTION-GOTO)) )

(define (REGULAR-CALL m m*)
  (append m (PUSH-VALUE) m* (POP-FUNCTION) 
          (PRESERVE-ENV) (FUNCTION-INVOKE) (RESTORE-ENV) ) )

(define (STORE-ARGUMENT m m* rank)
  (append m (PUSH-VALUE) m* (POP-FRAME! rank)) )

(define (CONS-ARGUMENT m m* arity)
  (append m (PUSH-VALUE) m* (POP-CONS-FRAME! arity)) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Instructions definers

(define-syntax define-instruction-set
  (syntax-rules (define-instruction)
    ((define-instruction-set
       (define-instruction (name . args) n . body) ... )
     (begin 
       (define (run)
         (let ((instruction (fetch-byte)))
           (case instruction
             ((n) (run-clause args body)) ... ) )
         (run) )
       (define (instruction-size code pc)
         (let ((instruction (vector-ref code pc)))
           (case instruction
             ((n) (size-clause args)) ... ) ) )
       (define (instruction-decode code pc)
         (define (fetch-byte)
           (let ((byte (vector-ref code pc)))
             (set! pc (+ pc 1))
             byte ) )
         (let-syntax
             ((decode-clause
               (syntax-rules ()
                 ((decode-clause iname ()) '(iname))
                 ((decode-clause iname (a)) 
                  (let ((a (fetch-byte))) (list 'iname a)) )
                 ((decode-clause iname (a b))
                  (let* ((a (fetch-byte))(b (fetch-byte)))
                    (list 'iname a b) ) ) )))
           (let ((instruction (fetch-byte)))
             (case instruction
               ((n) (decode-clause name args)) ... ) ) ) ) ) ) ) )

;;; This uses the global fetch-byte function that increments *pc*.

(define-syntax run-clause
  (syntax-rules ()
    ((run-clause () body) (begin . body))
    ((run-clause (a) body)
     (let ((a (fetch-byte))) . body) )
    ((run-clause (a b) body)
     (let* ((a (fetch-byte))(b (fetch-byte))) . body) ) ) )           

(define-syntax size-clause
  (syntax-rules ()
    ((size-clause ())    1)
    ((size-clause (a))   2)
    ((size-clause (a b)) 3) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Instruction-set. 
;;; The instructions are kept in a separate file (to be read by
;;; LiSP2TeX), the following macro reads them and generates a
;;; (define-instruction-set...) call.                       HACK!
;;; You can replace this paragraphe by:
;;;  (define-instruction-set <content of "src/chap7f.scm" file>...)

(define-abbreviation (definstructions filename)
  (define (read-file filename)
    (call-with-input-file filename
      (lambda (in)
        (let gather ((e (read in))
                     (content '()) )
          (if (eof-object? e)
              (reverse content)
              (gather (read in) (cons e content)) ) ) ) ) )
  (let ((content (read-file filename)) )
    (display '(reading instruction set ...))(newline)
    `(define-instruction-set . ,content) ) )

(definstructions "src/chap7f.scm")

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Combinators

(define (check-byte j)
  (unless (and (<= 0 j) (<= j 255))
    (static-wrong "Cannot pack this number within a byte" j) ) )
  
(define (SHALLOW-ARGUMENT-REF j)
  (check-byte j)
  (case j
    ((0 1 2 3) (list (+ 1 j)))
    (else      (list 5 j)) ) )

(define (PREDEFINED i)
  (check-byte i)
  (case i
    ;; 0=\#t, 1=\#f, 2=(), 3=cons, 4=car, 5=cdr, 6=pair?, 7=symbol?, 8=eq?
    ((0 1 2 3 4 5 6 7 8) (list (+ 10 i)))
    (else                (list 19 i)) ) )

(define (DEEP-ARGUMENT-REF i j) (list 6 i j))

(define (SET-SHALLOW-ARGUMENT! j)
  (case j
    ((0 1 2 3) (list (+ 21 j)))
    (else      (list 25 j)) ) )

(define (SET-DEEP-ARGUMENT! i j) (list 26 i j))

(define (GLOBAL-REF i) (list 7 i))

(define (CHECKED-GLOBAL-REF i) (list 8 i))

(define (SET-GLOBAL! i) (list 27 i))

(define (CONSTANT value)
  (cond ((eq? value #t)    (list 10))
        ((eq? value #f)    (list 11))
        ((eq? value '())   (list 12))
        ((equal? value -1) (list 80))
        ((equal? value 0)  (list 81))
        ((equal? value 1)  (list 82))
        ((equal? value 2)  (list 83))
        ((equal? value 4)  (list 84))
        ((and (integer? value)  ; immediate value
              (<= 0 value)
              (< value 255) )
         (list 79 value) )
        (else (EXPLICIT-CONSTANT value)) ) )

(define (EXPLICIT-CONSTANT value)
  (set! *quotations* (append *quotations* (list value)))
  (list 9 (- (length *quotations*) 1)) )

;;; All gotos have positive offsets (due to the generation)

(define (GOTO offset)
  (cond ((< offset 255) (list 30 offset))
        ((< offset (+ 255 (* 255 256))) 
         (let ((offset1 (modulo offset 256))
               (offset2 (quotient offset 256)) )
           (list 28 offset1 offset2) ) )
        (else (static-wrong "too long jump" offset)) ) )

(define (JUMP-FALSE offset)
  (cond ((< offset 255) (list 31 offset))
        ((< offset (+ 255 (* 255 256))) 
         (let ((offset1 (modulo offset 256))
               (offset2 (quotient offset 256)) )
           (list 29 offset1 offset2) ) )
        (else (static-wrong "too long jump" offset)) ) )

(define (EXTEND-ENV) (list 32))

(define (UNLINK-ENV) (list 33))

(define (INVOKE0 address)
  (case address
    ((read)    (list 89))
    ((newline) (list 88))
    (else (static-wrong "Cannot integrate" address)) ) )

(define (INVOKE1 address)
  (case address
    ((car)     (list 90))
    ((cdr)     (list 91))
    ((pair?)   (list 92))
    ((symbol?) (list 93))
    ((display) (list 94))
    (else (static-wrong "Cannot integrate" address)) ) )

;;; The same one with other unary primitives.
(define (INVOKE1 address)
  (case address
    ((car)     (list 90))
    ((cdr)     (list 91))
    ((pair?)   (list 92))
    ((symbol?) (list 93))
    ((display) (list 94))
    ((primitive?) (list 95))
    ((null?)   (list 96))
    ((continuation?) (list 97))
    ((eof-object?)   (list 98))
    (else (static-wrong "Cannot integrate" address)) ) )

(define (PUSH-VALUE) (list 34)) 

(define (POP-ARG1) (list 35))

(define (INVOKE2 address)
  (case address
    ((cons)     (list 100))
    ((eq?)      (list 101))
    ((set-car!) (list 102))
    ((set-cdr!) (list 103))
    ((+)        (list 104))
    ((-)        (list 105))
    ((=)        (list 106))
    ((<)        (list 107))
    ((>)        (list 108))
    ((*)        (list 109))
    ((<=)       (list 110))
    ((>=)       (list 111))
    ((remainder)(list 112))
    (else (static-wrong "Cannot integrate" address)) ) )

(define (POP-ARG2) (list 36))

(define (INVOKE3 address)
  (static-wrong "No ternary integrated procedure" address) )

(define (CREATE-CLOSURE offset) (list 40 offset))

(define (ARITY=? arity+1)
  (case arity+1
    ((1 2 3 4) (list (+ 70 arity+1)))
    (else        (list 75 arity+1)) ) )

(define (RETURN) (list 43))

(define (PACK-FRAME! arity) (list 44 arity))

(define (ARITY>=? arity+1) (list 78 arity+1))

(define (FUNCTION-GOTO) (list 46))

(define (POP-FUNCTION) (list 39))

(define (FUNCTION-INVOKE) (list 45))

(define (PRESERVE-ENV) (list 37))

(define (RESTORE-ENV) (list 38))

(define (POP-FRAME! rank)
  (case rank
    ((0 1 2 3) (list (+ 60 rank)))
    (else      (list 64 rank)) ) )

(define (POP-CONS-FRAME! arity) (list 47 arity))

(define (ALLOCATE-FRAME size)
  (case size
    ((0 1 2 3 4) (list (+ 50 size)))
    (else        (list 55 (+ size 1))) ) )

(define (ALLOCATE-DOTTED-FRAME arity) (list 56 (+ arity 1)))

(define (FINISH) (list 20))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Preserve the state of the machine ie the three environments.

(define (preserve-environment)
  (stack-push *env*) )

(define (restore-environment)
  (set! *env* (stack-pop)) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define (fetch-byte)
  (let ((byte (vector-ref *code* *pc*)))
    (set! *pc* (+ *pc* 1))
    byte ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Disassemble code

(define (disassemble code)
  (let loop ((result '())
             (pc 0) )
    (if (>= pc (vector-length code))
        (reverse! result)
        (loop (cons (instruction-decode code pc) result)
              (+ pc (instruction-size code pc)) ) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; If tail? is #t then the return address is on top of stack so no
;;; need to push another one.

(define-generic (invoke (f) tail?)
  (signal-exception #f (list "Not a function" f)) )

(define-method (invoke (f closure) tail?)
  (unless tail? (stack-push *pc*))
  (set! *env* (closure-closed-environment f))
  (set! *pc* (closure-code f)) )

(define-method (invoke (f primitive) tail?)
  (unless tail? (stack-push *pc*))
  ((primitive-address f)) )

(define-method (invoke (f continuation) tail?)
  (if (= (+ 1 1) (activation-frame-argument-length *val*))
      (begin
        (restore-stack (continuation-stack f))
        (set! *val* (activation-frame-argument *val* 0))
        (set! *pc* (stack-pop)) )
      (signal-exception #f (list "Incorrect arity" 'continuation)) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define-syntax defprimitive0
  (syntax-rules ()
    ((defprimitive0 name value)
     (definitial name
       (letrec ((arity+1 (+ 1 0))
                (behavior
                 (lambda ()
                   (if (= arity+1 (activation-frame-argument-length *val*))
                       (begin
                         (set! *val* (value))
                         (set! *pc* (stack-pop)) )
                       (signal-exception #t (list "Incorrect arity" 'name)) ) ) ) )
         (description-extend! 'name `(function value))
         (make-primitive behavior) ) ) ) ) )
  
(define-syntax defprimitive1
  (syntax-rules ()
    ((defprimitive1 name value)
     (definitial name
       (letrec ((arity+1 (+ 1 1))
                (behavior
                 (lambda ()
                   (if (= arity+1 (activation-frame-argument-length *val*))
                       (let ((arg1 (activation-frame-argument *val* 0)))
                         (set! *val* (value arg1))
                         (set! *pc* (stack-pop)) )
                       (signal-exception #t (list "Incorrect arity" 'name)) ) ) ) )
         (description-extend! 'name `(function value a))
         (make-primitive behavior) ) ) ) ) )
  
(define-syntax defprimitive2
  (syntax-rules ()
    ((defprimitive2 name value)
     (definitial name
       (letrec ((arity+1 (+ 2 1))
                (behavior
                 (lambda ()
                   (if (= arity+1 (activation-frame-argument-length *val*))
                       (let ((arg1 (activation-frame-argument *val* 0))
                             (arg2 (activation-frame-argument *val* 1)) )
                         (set! *val* (value arg1 arg2))
                         (set! *pc* (stack-pop)) )
                       (signal-exception #t (list "Incorrect arity" 'name)) ) ) ) )
         (description-extend! 'name `(function value a b))
         (make-primitive behavior) ) ) ) ) )

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defprimitive pair? pair? 1)
(defprimitive symbol? symbol? 1)
(defprimitive eq? eq? 2)
(defprimitive set-car! set-car! 2)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defprimitive = = 2)
(defprimitive < < 2)
(defprimitive > > 2)
(defprimitive * * 2)
(defprimitive <= <= 2)
(defprimitive >= >= 2)
(defprimitive remainder remainder 2)
(defprimitive display display 1)
(defprimitive read read 0)
(defprimitive primitive? primitive? 1)
(defprimitive continuation? continuation? 1)
(defprimitive null? null? 1)
(defprimitive newline newline 0)
(defprimitive eof-object? eof-object? 1)

;;; The function which is invoked by call/cc always waits for an
;;; activation frame. 

(definitial call/cc
  (let* ((arity 1)
         (arity+1 (+ arity 1)) )
    (make-primitive
     (lambda ()
       (if (= arity+1 (activation-frame-argument-length *val*))
           (let ((f (activation-frame-argument *val* 0))
                 (frame (allocate-activation-frame (+ 1 1))) )
             (set-activation-frame-argument! 
              frame 0 (make-continuation (save-stack)) )
             (set! *val* frame)
             (set! *fun* f)             ; useful for debug
             (invoke f #t) )
           (signal-exception #t (list "Incorrect arity" 
                                      'call/cc )) ) ) ) ) )

(definitial apply
  (let* ((arity 2)
         (arity+1 (+ arity 1)) )
    (make-primitive
     (lambda ()
       (if (>= (activation-frame-argument-length *val*) arity+1)
           (let* ((proc (activation-frame-argument *val* 0))
                  (args-number (activation-frame-argument-length *val*))
                  (last-arg-index (- args-number 2))
                  (last-arg (activation-frame-argument *val* last-arg-index))
                  (size (+ last-arg-index (length last-arg)))
                  (frame (allocate-activation-frame size)) )
             (do ((i 1 (+ i 1)))
                 ((= i last-arg-index))
               (set-activation-frame-argument! 
                frame (- i 1) (activation-frame-argument *val* i) ) )
             (do ((i (- last-arg-index 1) (+ i 1))
                  (last-arg last-arg (cdr last-arg)) )
                 ((null? last-arg))
               (set-activation-frame-argument! frame i (car last-arg)) )
             (set! *val* frame)
             (set! *fun* proc)  ; useful for debug
             (invoke proc #t) )
           (signal-exception #f (list "Incorrect arity" 'apply)) ) ) ) ) )

(definitial list
  (make-primitive
   (lambda ()
     (let ((args-number (- (activation-frame-argument-length *val*) 1))
           (result '()) )
       (do ((i args-number (- i 1)))
           ((= i 0))
         (set! result (cons (activation-frame-argument *val* (- i 1)) 
                            result )) ) 
       (set! *val* result)
       (set! *pc* (stack-pop)) ) ) ) )

;;; Reserve some variables for future use in future chapters.

(define-syntax defreserve
  (syntax-rules ()
    ((defreserve name)
     (definitial name
       (make-primitive
        (lambda ()
          (signal-exception #f (list "Not yet implemented" 'name)) ) ) ) ) ) )

(defreserve global-value)
(defreserve load)
(defreserve eval)
(defreserve eval/at)
(defreserve eval/b)
(defreserve enrich)
(defreserve procedure->environment)
(defreserve procedure->definition)
(defreserve variable-value)
(defreserve set-variable-value!)
(defreserve variable-defined?)

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Use Meroon show functions to describe the inner working.

(define *debug* #f)

(define (show-registers message)
  (when *debug* 
    (format #t "~%----------------~A" message)
    (format #t "~%ENV  = ") (show *env*)
    (format #t "~%VAL  = ") (show *val*)
    (format #t "~%FUN  = ") (show *fun*)
    (show-stack (save-stack))
    (format #t "~%(PC  = ~A), next INSTR to be executed = ~A~%" 
            *pc* (instruction-decode *code* *pc*) ) ) )

(define (show-stack stack)
  (let ((n (vector-length stack)))
    (do ((i 0 (+ i 1)))
        ((= i n))
      (format #t "~%STK[~A]= " i)(show (vector-ref *stack* i)) ) ) )

(define-method (show (f closure) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (format stream "#<Closure(pc=~A)>" (closure-code f)) ) )

(define-method (show (a activation-frame) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "[Frame next=" stream)
    (show (activation-frame-next a) stream)
    (display ", content=" stream)
    (do ((i 0 (+ 1 i)))
        ((= i (activation-frame-argument-length a)))
      (show (activation-frame-argument a i) stream)
      (display " & " stream) )
    (display "]" stream) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define (code-prologue)
  (set! finish-pc 0)
  (FINISH) )

(define (make-code-segment m)
  (apply vector (append (code-prologue) m (RETURN))) )

(define (chapter7d-interpreter)
  (define (toplevel)
    (display ((stand-alone-producer7d (read)) 100))
    (toplevel) )
  (toplevel) ) 

(define (stand-alone-producer7d e)
  (set! g.current (original.g.current))
  (set! *quotations* '())
  (let* ((code (make-code-segment (meaning e r.init #t)))
         (start-pc (length (code-prologue)))
         (global-names (map car (reverse g.current)))
         (constants (apply vector *quotations*)) )
    (lambda (stack-size)
      (run-machine stack-size start-pc code 
                   constants global-names ) ) ) )

(define (run-machine stack-size pc code constants global-names)
  (set! sg.current (make-vector (length global-names) 
                                undefined-value ))
  (set! sg.current.names global-names)
  (set! *constants*   constants)
  (set! *code*        code)
  (set! *env*         sr.init)
  (set! *stack*       (make-vector stack-size))
  (set! *stack-index* 0)
  (set! *val*         'anything)
  (set! *fun*         'anything)
  (set! *arg1*        'anything)
  (set! *arg2*        'anything)
  (stack-push finish-pc)                ;  pc for FINISH
  (set! *pc*          pc)
  (call/cc (lambda (exit)
             (set! *exit* exit)
             (run) )) )

;;; Patch run to show registers in debug mode.

(let ((native-run run))
  (set! run (lambda ()
              (when *debug* (show-registers ""))
              (native-run) )) )
(let ((native-run-machine run-machine))
  (set! run-machine
        (lambda (stack-size pc code constants global-names)
          (when *debug*                     ; DEBUG
            (format #t "Code= ~A~%" (disassemble code)) )         
          (native-run-machine stack-size pc code constants global-names) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Tests

(define (scheme7d)
  (interpreter
   "Scheme? "  
   "Scheme= " 
   #t
   (lambda (read print error)
     (setup-wrong-functions error)
     (lambda ()
       ((stand-alone-producer7d (read)) 100)
       (print *val*) ) ) ) )

(define (test-scheme7d file)
  (suite-test 
   file 
   "Scheme? " 
   "Scheme= "
   #t
   (lambda (read check error)
     (setup-wrong-functions error)
     (lambda ()
       ((stand-alone-producer7d (read)) 100)
       (check *val*) ) )
   equal? ) )

(define (setup-wrong-functions error)
  (set! signal-exception (lambda (c . args) (apply error args)))
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

;;; Missing global variables

(define signal-exception 'wait)
(define finish-pc 'wait)
(define *exit* 'wait)

;;; end of chap7d.scm
