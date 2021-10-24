;;; $Id: chap10e.scm,v 4.3 2006/11/27 12:13:21 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; C generation (look at chap6f.scm)
;;; Requires chap10{a,c,g,h}.scm !
;;; Load after chap10h.scm to define the predefined library.

(define (compile->C e out)
  (set! g.current '())
  (let ((prg (extract-things! (lift! (Sexp->object e)))))
    (gather-temporaries! (closurize-main! prg))
    (generate-C-program out e prg) ) )

(define (generate-C-program out e prg)
  (generate-header out e)
  (generate-global-environment out g.current)
  (generate-quotations out (Flattened-Program-quotations prg))
  (generate-functions out (Flattened-Program-definitions prg))
  (generate-main out (Flattened-Program-form prg))
  (generate-trailer out)
  prg )

;;; A free variable is defined to be a global variable defined on the fly.
;;; Dont forget to skip associated tests.

(define (objectify-free-global-reference name r)
  (let ((v (make-Global-Variable name)))
    (set! g.current (cons v g.current))
    (make-Global-Reference v) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;;              Generation

;;; File header and trailer are simple.

(define (generate-header out e)
  (format out "/* Compiler to C $Revision: 4.3 $ ~%")
  (pp e out)                            ; DEBUG
  (format out "  */~%~%#include \"scheme.h\"~%") )

(define (generate-trailer out)
  (format out "~%/* End of generated code. */~%") )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Generate global environment. For all Global-Variables allocate a
;;; location to hold it. We also give the real name to the macro so
;;; that some reflective capabilities can be added to C.

;;; gv* is a list of instances of Global-Variable.

(define (generate-global-environment out gv*)
  (when (pair? gv*)
    (format out "~%/* Global environment: */~%")
    (for-each (lambda (gv) (generate-global-variable out gv))
              gv* )) )

(define (generate-global-variable out gv)
  (let ((name (Global-Variable-name gv)))
    (format out "SCM_DefineGlobalVariable(~A,\"~A\");~%"
            (IdScheme->IdC name) name ) ) )

;;; Convert a Scheme name to a C name. All predefined names will be
;;; inserted in the following map so they can be uniformly named. The
;;; map is an Alist ( (symbol . "Cname") ... ).

(define Scheme->C-names-mapping 
  '( (*        . "TIMES")
     (<        . "LESSP")
     (pair?    . "CONSP")
     (set-cdr! . "RPLACD")
     ) )

(set! Scheme->C-names-mapping 
  '( (*        . "TIMES")
     (+        . "PLUS")
     (-        . "DIFFERENCE")
     (/        . "QUOTIENT")
     (>        . "GREATERP")
     (>=       . "NOT_LESSP")
     (<        . "LESSP")
     (<=       . "NOT_GREATERP")
     (=        . "EQN")
     (eq?      . "EQ")
     (pair?    . "CONSP")
     (symbol?  . "SYMBOLP")
     (set-car! . "RPLACA")
     (set-cdr! . "RPLACD")
     ) )

(define (IdScheme->IdC name)
  (let ((v (assq name Scheme->C-names-mapping)))
    (if (pair? v) (cdr v)
        (let ((str (symbol->string name)))
          (let retry ((Cname (compute-Cname str)))
            (if (Cname-clash? Cname Scheme->C-names-mapping)
                (retry (compute-another-Cname str))
                (begin (set! Scheme->C-names-mapping
                             (cons (cons name Cname) 
                                   Scheme->C-names-mapping ) )
                       Cname ) ) ) ) ) ) )

(define (Cname-clash? Cname mapping)
  (let check ((mapping mapping))
    (and (pair? mapping)
         (or (string=? Cname (cdr (car mapping)))
             (check (cdr mapping)) ) ) ) )

;;; These functions compute a C name for a symbol. Scheme symbols
;;; cannot be transleted into a name containing an isolated underscore
;;; so all these names will be used for C generation purposes.

(define compute-another-Cname 
  (let ((counter 1))
    (lambda (str)
      (set! counter (+ 1 counter))
      (compute-Cname (format #f "~A_~A" str counter)) ) ) )

(define (compute-Cname str)
  (define (mapcan f l)
    (if (pair? l)
        (append (f (car l)) (mapcan f (cdr l)))
        '() ) )
  (define (convert-char char)
    (case char
      ((#\_)             '(#\_ #\_))
      ((#\?)             '(#\p))
      ((#\!)             '(#\i))
      ((#\<)             '(#\l))
      ((#\>)             '(#\g))
      ((#\=)             '(#\e))
      ((#\- #\/ #\* #\:) '())
      (else              (list char)) ) )
  (let ((cname (mapcan convert-char (string->list str))))
    (if (pair? cname) (list->string cname) "weird") ) )

;(define (compute-Cname str)
;  (let ((n (string-length str))
;        (result '()) )
;    (do ((i 0 (+ 1 i)))
;        ((= i n)
;         (if (pair? result)
;             (list->string (reverse result))
;             "_weird_" ) )
;      (set! result 
;            (append
;             (let ((char (string-ref str i)))
;               (case char
;                 ((#\_) '(#\_ #\_))
;                 ((#\?) '(#\p))
;                 ((#\!) '(#\i))
;                 ((#\<) '(#\l))
;                 ((#\>) '(#\g))
;                 ((#\=) '(#\e))
;                 ((#\- #\/ #\* #\:) '())
;                 (else (list char)) ) )
;             result ) ) ) ) )
; (cons (IdScheme->IdC 'foo) Scheme->C-names-mapping)
; (cons (IdScheme->IdC 'foo-bar) Scheme->C-names-mapping)
; (cons (IdScheme->IdC 'foo_bar) Scheme->C-names-mapping)
; (cons (IdScheme->IdC 'set!-f*/*-<b:ar>?) Scheme->C-names-mapping)
; (cons (IdScheme->IdC 'set!-f*/*-<bar>?) Scheme->C-names-mapping)
; Do not handle 1+  !!!

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Generate static shared quotations. Linearize quotations so they
;;; can be expressed by C constants. Equal? quotations are shared.
;;; And quotations here are completely static.

;;; qv* is a list of instances of Quotation-Variable.

(define (generate-quotations out qv*)
  (when (pair? qv*)
    (format out "~%/* Quotations: */~%")
    (scan-quotations out qv* (length qv*) '()) ) )

;;;  Does not handle vectors.

(define (scan-quotations out qv* i results)
  (when (pair? qv*)
    (let* ((qv       (car qv*))
           (value    (Quotation-Variable-value qv))
           (other-qv (already-seen-value? value results)) )
      (cond (other-qv 
             (generate-quotation-alias out qv other-qv)
             (scan-quotations out (cdr qv*) i (cons qv results)) )
            ((C-value? value) 
             (generate-C-value out qv)
             (scan-quotations out (cdr qv*) i (cons qv results)) )
            ((symbol? value)
             (scan-symbol out value qv* i results) )
            ((pair? value)
             (scan-pair out value qv* i results) )
            (else (generate-error "Unhandled constant" qv)) ) ) ) )

(define (scan-pair out value qv* i results)
  (let* ((qv  (car qv*))
         (d   (cdr value))
         (dqv (already-seen-value? d results)) )
    (if dqv
        (let* ((a   (car value))
               (aqv (already-seen-value? a results)) )
          (if aqv
              (begin 
                (generate-pair out qv aqv dqv)
                (scan-quotations out (cdr qv*) i (cons qv results)) )
              (let ((newaqv (make-Quotation-Variable i a)))
                (scan-quotations out (cons newaqv qv*)
                                 (+ i 1) results ) ) ) )
        (let ((newdqv (make-Quotation-Variable i d)))
          (scan-quotations 
           out (cons newdqv qv*) (+ i 1) results ) ) ) ) )

(define (generate-pair out qv aqv dqv)
  (format out 
   "SCM_DefinePair(thing~A_object,thing~A,thing~A); /* ~S */~%"
   (Quotation-Variable-name qv)
   (Quotation-Variable-name aqv)
   (Quotation-Variable-name dqv)
   (Quotation-Variable-value qv) )
  (format out "#define thing~A SCM_Wrap(&thing~A_object)~%"
          (Quotation-Variable-name qv) 
          (Quotation-Variable-name qv) ) )

(define (scan-symbol out value qv* i results)
  (let* ((qv    (car qv*))
         (str   (symbol->string value))
         (strqv (already-seen-value? str results)) )
    (cond (strqv (generate-symbol out qv strqv)
                 (scan-quotations out (cdr qv*) i (cons qv results)) )
          (else
           (let ((newqv (make-Quotation-Variable 
                         i (symbol->string value) )))
             (scan-quotations out (cons newqv qv*) 
                              (+ i 1) results ) ) ) ) ) )

(define (generate-symbol out qv strqv)
  (format out "SCM_DefineSymbol(thing~A_object,thing~A);  /* ~S */~%"
          (Quotation-Variable-name qv)
          (Quotation-Variable-name strqv)
          (Quotation-Variable-value qv) )
  (format out "#define thing~A SCM_Wrap(&thing~A_object)~%"
          (Quotation-Variable-name qv) 
          (Quotation-Variable-name qv) ) )

(define (generate-quotation-alias out qv1 qv2)
  (format out "#define thing~A thing~A	/* ~S */~%"
          (Quotation-Variable-name qv1)
          (Quotation-Variable-name qv2)
          (Quotation-Variable-value qv2) ) )

;;; Equal? quotations are shared.

(define (already-seen-value? value qv*)
  (and (pair? qv*)
       (if (equal? value (Quotation-Variable-value (car qv*)))
           (car qv*)
           (already-seen-value? value (cdr qv*)) ) ) )
  
;;; should be tailored to the C compiler. This does say that we use
;;; complement to 1.

(define *maximal-fixnum* 16384)
(define *minimal-fixnum* (- *maximal-fixnum*))

(define (C-value? value)
  (or (null? value)
      (boolean? value)
      (and (integer? value)
           (< *minimal-fixnum* value)
           (< value *maximal-fixnum*) )
      (string? value) ) )

(define (generate-C-value out qv)
  (let ((value (Quotation-Variable-value qv))
        (index (Quotation-Variable-name qv)) )
    (cond ((null? value)
           (format out "#define thing~A SCM_nil	/* () */~%"
                   index ) )
          ((boolean? value)
           (format out "#define thing~A ~A	/* ~S */~%"
                   index (if value "SCM_true" "SCM_false") value ) )
          ((integer? value)
           (format out "#define thing~A SCM_Int2fixnum(~A)~%"
                   index value ) )
          ((string? value)
           (format out "SCM_DefineString(thing~A_object,\"~A\");~%"
                   index value )
           (format out "#define thing~A SCM_Wrap(&thing~A_object)~%"
                   index index ) ) ) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; To ease writing printing expressions.

(define-syntax between-parentheses 
  (syntax-rules ()
    ((between-parentheses out . body)
     (let ((out out))
       (format out "(")
       (begin . body)
       (format out ")") ) ) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Convert an expression to C
;;; Faire en une passe  plutot qu'avec  des chaines.

(define-generic (->C (e Program) out))

;;; Generate the name used in C for a variable.

(define-generic (variable->C (variable) out))

(define-method (variable->C (variable Variable) out)
  (format out (IdScheme->IdC (Variable-name variable))) )

(define-method (variable->C (variable Renamed-Local-Variable) out)
  (format out "~A_~A" 
          (IdScheme->IdC (Variable-name variable))
          (Renamed-Local-Variable-index variable) ) )

(define-method (variable->C (variable Quotation-Variable) out)
  (format out "thing~A" (Quotation-Variable-name variable)) )

;;; Works for Local-, Global-, Predefined- References but for Free-References.

(define-method (->C (e Reference) out)
  (reference->C (Reference-variable e) out) )

(define-generic (reference->C (v Variable) out))

(define-method (reference->C (v Variable) out)
  (variable->C v out) )

;;; Exercice: change this to produce a more user-friendly error message.

(define-method (reference->C (v Global-Variable) out)
  (format out "SCM_CheckedGlobal")
  (between-parentheses out
    (variable->C v out) ) )

(define-method (->C (e Free-Reference) out)
  (format out "SCM_Free")
  (between-parentheses out
    (variable->C (Free-Reference-variable e) out) ) )

(define-method (->C (e Global-Assignment) out)
  (between-parentheses out
    (variable->C (Global-Assignment-variable e) out)
    (format out "=")
    (->C (Global-Assignment-form e) out) ) ) 

(define-method (->C (e Box-Read) out)
  (format out "SCM_Content")
  (between-parentheses out
    (->C (Box-Read-reference e) out) ) )

(define-method (->C (e Box-Write) out)
  (between-parentheses out
    (format out "SCM_Content")
    (between-parentheses out
      (->C (Box-Write-reference e) out) )
    (format out "=")
    (->C (Box-Write-form e) out) ) )

(define-method (->C (e Box-Creation) out)
  (variable->C (Box-Creation-variable e) out)
  (format out "= SCM_allocate_box")
  (between-parentheses out
    (variable->C (Box-Creation-variable e) out) ) )

;;;  distinguer qqs  cas comme  eq?, ou pair?

(define-method (->C (e Alternative) out)
  (between-parentheses out
    (boolean->C (Alternative-condition e) out)
    (format out "~%? ")
    (->C (Alternative-consequent e) out)
    (format out "~%: ")
    (->C (Alternative-alternant e) out) ) )

(define-generic (boolean->C (e) out)
  (between-parentheses out
    (->C e out)
    (format out " != SCM_false") ) )
    
(define-method (->C (e Sequence) out)
  (between-parentheses out
    (->C (Sequence-first e) out)
    (format out ",~%")
    (->C (Sequence-last e) out) ) )

;;; (define-method (->C (e Constant) ...) does not exist any longer.

(define-method (->C (e Regular-Application) out)
  (let ((n (number-of (Regular-Application-arguments e))))
    (cond ((< n 4)
           (format out "SCM_invoke~A" n)
           (between-parentheses out
             (->C (Regular-Application-function e) out)
             (->C (Regular-Application-arguments e) out) ) )
          (else 
           (format out "SCM_invoke")
           (between-parentheses out
             (->C (Regular-Application-function e) out)
             (format out ",~A" n)
             (->C (Regular-Application-arguments e) out) ) ) ) ) )

(define-method (->C (e Arguments) out)
  (format out ",~%")
  (->C (Arguments-first e) out)
  (->C (Arguments-others e) out) )

(define-method (->C (e No-Argument) out)
  #t )

(define-method (->C (e Fix-Let) out)
  (between-parentheses out
    (bindings->C (Fix-Let-variables e) (Fix-Let-arguments e) out)
    (->C (Fix-Let-body e) out) ) )

(define-generic (bindings->C variables (arguments) out))

(define-method (bindings->C variables (e Arguments) out)
  (variable->C (car variables) out)
  (format out "=")
  (->C (Arguments-first e) out)
  (format out ",~%")
  (bindings->C (cdr variables) (Arguments-others e) out) )

(define-method (bindings->C variables (e No-Argument) out)
  (format out "") )

(define-method (->C (e Predefined-Application) out)
  ((Functional-Description-generator
    (Predefined-Variable-description
     (Predefined-Application-variable e) ) ) e out ) )

(define (make-predefined-application-generator Cname)
  (lambda (e out)
    (format out "~A" Cname)
    (between-parentheses out
      (arguments->C (Predefined-Application-arguments e) out) ) ) )

(define-generic (arguments->C (e) out))

(define-method (arguments->C (e Arguments) out)
  (->C (Arguments-first e) out)
  (->C (Arguments-others e) out) )

(define-method (arguments->C (e No-Argument) out)
  #t )

;;; Exercice: Close statically if there are no free variables.

(define-method (->C (e Closure-Creation) out)
  (format out "SCM_close")
  (between-parentheses out
    (format out "SCM_CfunctionAddress(function_~A),~A,~A" 
            (Closure-Creation-index e)
            (generate-arity (Closure-Creation-variables e))
            (number-of (Closure-Creation-free e)) )
    (->C (Closure-Creation-free e) out) ) )

(define (generate-arity variables)
  (let count ((variables variables)(arity 0))
    (if (pair? variables)
        (if (Local-Variable-dotted? (car variables))
            (- (+ arity 1))
            (count (cdr variables) (+ 1 arity)) )
        arity ) ) )

(define-method (->C (e No-Free) out)
  #t )

(define-method (->C (e Free-Environment) out)
  (format out ",")
  (->C (Free-environment-first e) out)
  (->C (Free-environment-others e) out) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Count terms

(define-method (number-of (o Free-Environment))
  (+ 1 (number-of (Free-Environment-others o))) )

(define-method (number-of (o No-Free)) 0)

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Function definitions

(define (generate-functions out definitions)
  (format out "~%/* Functions: */~%")
  (for-each (lambda (def) 
              (generate-closure-structure out def)
              (generate-possibly-dotted-definition out def) )
            (reverse definitions) ) )

;;; It is possible to overload the word function_i to be a struct as
;;; well as a function in C.

;(define (generate-closure-structure out definition)
;  (format out "struct function_~A {~%"
;          (Function-Definition-index definition) )
;  (format out " SCM (*behavior)();~% int arity;")
;  (for-each (lambda (v) 
;              (format out "~% SCM ")
;              (variable->C v out)
;              (format out ";") )
;            (Function-Definition-free definition) )
;  (format out "~%};~%") )

(define (generate-closure-structure out definition)
  (format out "SCM_DefineClosure(function_~A, "
          (Function-Definition-index definition) )
  (generate-local-temporaries (Function-Definition-free definition) 
                              out )
  (format out ");~%") )

(define (generate-possibly-dotted-definition out definition)
  (format out "~%SCM_DeclareFunction(function_~A) {~%"
          (Function-Definition-index definition) )
  (let ((vars (Function-Definition-variables definition))
        (rank -1) )
    (for-each 
     (lambda (v)
       (set! rank (+ rank 1))
       (cond ((Local-Variable-dotted? v)
              (format out "SCM_DeclareLocalDottedVariable(") )
             ((Variable? v)
              (format out "SCM_DeclareLocalVariable(") ) )
       (variable->C v out)
       (format out ",~A);~%" rank) )
     vars )
    (let ((temps (With-Temp-Function-Definition-temporaries 
                  definition )))
      (when (pair? temps) 
        (generate-local-temporaries temps out)
        (format out "~%") ) )
    (format out "return ")
    (->C (Function-Definition-body definition) out)
    (format out ";~%}~%~%") ) )

(define (generate-local-temporaries temps out)
  (when (pair? temps)
    (format out "SCM ")
    (variable->C (car temps) out)
    (format out "; ")
    (generate-local-temporaries (cdr temps) out) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Main procedure. We choose to print the result.

(define (generate-main out form)
  (format out "~%/* Expression: */~%void main(void) {~%")
  (format out "  SCM_print")
  (between-parentheses out
    (->C form out) )
  (format out ";~%  exit(0);~%}~%") )

;;; global variables.

(define g.current '())

;;; end of chap10e.scm
