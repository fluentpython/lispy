;;; $Id: chap6f.scm,v 4.0 1995/07/10 06:51:43 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;;                 Small compiler to C

;;; Environment is held by a global variable. This is bad for //ism.
;;; Continuation are now implicit and call/cc is a magical operator.
;;; Also try to introduce combinators as much as possible.
;;; Closures are explicitely represented.

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Determine the nature of a variable.
;;; Three different answers. Or the variable is local (ie appears in SR)
;;; then return     (LOCAL index . depth)
;;; global (ie created by the user) then return
;;;                 (GLOBAL . index)
;;; or predefined (and immutable) then return
;;;                 (PREDEFINED . index)
;;; This function comes from chap6b.scm

(define (compute-kind sr n)
  (define (local-variable? sr i)
    (and (pair? sr)
         (let scan ((names (car sr))
                    (j 1) )
           (cond ((pair? names) 
                  (if (eq? n (car names))
                      `(local ,i . ,j)
                      (scan (cdr names) (+ 1 j)) ) )
                 ((null? names)
                  (local-variable? (cdr sr) (+ i 1)) )
                 ((eq? n names) `(local ,i . ,j)) ) ) ) )
  (define (global-variable? sg)
    (let ((var (assq n sg)))
      (and (pair? var)
           (cdr var) ) ) )
  ;; test in order
  (or (local-variable? sr 0)
      (global-variable? sg.current)
      (global-variable? sg.init)
      (adjoin-global-variable! n) ) )

;;; The new variable is stored at the beginning of sg.current.

(define (adjoin-global-variable! name)
  (let ((index (sg.current-extend! name)))
    (cdr (car sg.current)) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; SR is the static representation of the runtime local environment.
;;; It is represented by a list of list of variables (the classical
;;; rib cage). 

(define (sr-extend* sr n*)
  (cons n* sr) )

(define sr.init '())

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; User-defined global environment definition. This environment is
;;; initially completely empty and can be extended by the user.
;;; It actually tolerates only 100 new global variables.

;;; SG.CURRENT represents the `static' user-defined global environment. 
;;; It is represented by the list of the symbols naming these global
;;; variables. Their values are held in the G.CURRENT vector.

(define sg.current '())

(define g.current (make-vector 100))

(define (sg.current-extend! n)
  (let ((level (length sg.current)))
    (set! sg.current 
          (cons (cons n `(global . ,level)) sg.current) )
    level ) )

(define (sg.current-initialize! name)
  (let ((kind (compute-kind sr.init name)))
    (if kind
        (case (car kind)
          ((global) #f)
          (else (static-wrong "Wrong redefinition" name)) )
        (let ((index (sg.current-extend! name)))
          #f ) ) )
  name )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Predefined global environment definition. This global environment
;;; is immutable. SG.INIT represents the static predefined global
;;; environment and is represented by the list of the symbols naming
;;; these global variables. Their values are held in the G.INIT vector.

(define sg.init '())

(define (sg.init-extend! n)
  (let ((level (length sg.init)))
    (set! sg.init
          (cons (cons n `(predefined . ,level)) sg.init) )
    ;; Register its C name
    (IdScheme->IdC n)
    level ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Describe a predefined value.
;;; The description language only represents primitives with their arity:
;;;          (FUNCTION address . variables-list)
;;; with variables-list := () | (a) | (a b) | (a b c)
;;; Only the structure of the VARIABLES-LIST is interesting (not the
;;; names of the variables). ADDRESS is the address of the primitive
;;; to use when inlining an invokation to it. This address is
;;; represented by a Scheme procedure.

(define desc.init '())

(define (description-extend! name description)
  (set! desc.init 
        (cons (cons name description) desc.init) )
  name )

;;; Return the description or #f if absent.

(define (get-description name)
  (let ((p (assq name desc.init)))
    (and (pair? p) (cdr p)) ) )
        
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooNEW
;;; Compilation 

;;; All met quotations are gathered here since they will be handled at
;;; toplevel at the beginning of the generated file.

(define *quotations* '())

;;; All met closures are gathered here for the same reasons.

(define *closures* '())

(define (generate-header out e)
  (format out "/* Expression compiled by chap6f $Revision: 4.0 $:~%")
  (pp e out) ; DEBUG
  (format out "  */

/* Standard prelude: */
#include \"rt.h\" " ) )

(define (generate-user-global-variables out)
  (when (pair? sg.current)
    (format out "
/* User global variables:  */" )
    (for-each (lambda (d)
                (let ((name (car d)))
                  (format out "
SCM SCM_~A = (SCM) &(SCM_undef_object) ;" 
                          (convert-string-uppercase
                           (symbol->string name) ) ) ) )
              sg.current ) )
  (format out "~%") )

(define (generate-local-temporaries out mvars)
  (when (pair? mvars)
    (format out "~%~A" (local-temporaries mvars)) ) )
    
(define (generate-closure-bodies out closures)
  (when (pair? closures)
    (format out "
/* Functions:  */" )
    (for-each (lambda (d)
                (let ((name (car d))
                      (code (cdr d)) )
                  (format out "~%~A" code) ) )
              closures ) )
  (format out "~%") )

(define (generate-expression out m)
  (let ((mcode (car m))
        (mvars (cdr m)) )
    (format out "
static SCM 
expression () { " )
  (generate-local-temporaries out mvars)
  (format out "
~A;
}
" mcode ) ) )    

(define (generate-main out)
  ;; generate header
  (format out "
/* Entry point:  */
void main(int argc, char *argv[]) { ")
  (format out "
/* Mark the (approximate) bottom of the stack */
char *current_stack_ptr ;
SCM_Cstack_bottom = (void*) &current_stack_ptr ;
SCM_initialize(argc,argv) ;
expression();
exit(0);
}
" ) )

(define (generate-postlude out)
  (format out "~%/* End of generated code. */~%") )

(define (compile->C out e)
  (set! *quotations* '())
  (set! *closures* '())
  (set! sg.current '())
  (let ((m (meaning e sr.init #t)))
    (generate-header out e)
    (generate-user-global-variables out)
    (generate-quotations out *quotations*)
    ;; C requires functions to be defined in the correct order or,
    ;; alternatively to define prototypes.
    (generate-closure-bodies out (reverse *closures*))
    (generate-expression out m)
    (generate-main out)
    (generate-postlude out) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooNEW
;;; generation of quotations. Make them as static as possible.

(define (generate-quotations out quotations)
  (when (pair? quotations)
    (set! *quotations* (reverse! (linearize-quotations quotations)))
    (format out "
/* Quotations: */")
    (do ((i 0 (+ i 1))
         (q* *quotations* (cdr q*)) )
        ((null? q*))
      (let* ((d (car q*))
             (value (car d))
             (names (cdr d)) 
             (reference (generate-quotation-value out value i)) )
        (for-each (lambda (name) 
                    (format out  "
static SCM ~A = (SCM) &(~A) ;" name reference ) )
                  names ) ) )
    (format out "~%") ) )

;;; NOTE: It may seem curious that I use equal instead of eq?. The
;;; reason is that symbol->string is not required to have eq results.
;;; (eq? (symbol->string 'foo)(symbol->string 'foo)) -> ?

(define (get-reference value)
  (let scan ((i 0)(q* *quotations*))
    (if (pair? q*)
        (if (equal? (car (car q*)) value)
            i
            (scan (+ i 1) (cdr q*)) )
        (static-wrong "Lost quotation" value) ) ) )

(define (generate-quotation-value out value index)
  (cond ((null? value) 
         (let ((reference "SCM_empty_object"))
           (format out "
#define thing~A (~A)" index reference )
           reference ) )
        ((boolean? value)
         (let ((reference 
                (format #f "SCM_~A_object" (if value "true" "false")) ))
           (format out "
#define thing~A (~A)" index reference ) ) )

        ((string? value)
         (let ((reference (format #f "thing~A" index)))
           (format out "
static struct SCM_string ~A =
  {{SCM_STRING_TAG, (SCM) NULL}, SIZEOF(\"~A\")-1, 
   \"~A\" } ;" 
                   reference value value )
           reference ) )

        ((pair? value)
         (let ((car-index (get-reference (car value)))
               (cdr-index (get-reference (cdr value)))
               (reference (format #f "thing~A" index)) )
           (format out "
static struct SCM_pair ~A =
  {{SCM_PAIR_TAG, (SCM) NULL}, (SCM) &(thing~A), (SCM) &(thing~A)} ;"
                   reference cdr-index car-index )
           reference ) )

        ((symbol? value)
         (let ((string-index (get-reference (symbol->string value)))
               (reference (format #f "thing~A" index)) )
           (format out "
static struct SCM_symbol ~A =
  {{SCM_SYMBOL_TAG, (SCM) NULL}, (SCM) &(thing~A)} ;" 
                   reference string-index )
           reference ) )

        ((integer? value)
         (let ((reference (format #f "thing~A" index)))
           (format out "
static struct SCM_fixnum ~A =
  {{SCM_FIXNUM_TAG, (SCM) NULL}, ~A} ;"
                   reference value )
           reference ) )

        (else (static-wrong "Unknown type of quotation" value)) ) )

;;; can handle cycles! (require prototypes in C)      ADD TESTS !!
;;; Quotations is a list of pairs (value name)
;;; results is a list of records (value name ...)

(define (linearize-quotations quotations)
  (define (assoc value results)
    (if (pair? results)
        (if (or (eqv? value (car (car results)))
                (equal? value (car (car results))) )
            (car results)
            (assoc value (cdr results)) )
        #f ) )
  (let scan ((results '())
             (quotations quotations) )
    (if (pair? quotations)
        (let* ((q (car quotations))
               (value (car q))
               (names (cdr q)) )
          (let ((record (assoc value results)))
            (cond ((pair? record)
                   (set-cdr! record (append names (cdr record)))
                   (scan results (cdr quotations)) )
                  ((or (null? value)
                       (boolean? value)
                       (and (integer? value)
                            (< *minimal-value* value)
                            (< value *maximal-fixnum*) )
                       (string? value) )
                   (scan (cons q results) (cdr quotations)) )
                  ((symbol? value)
                   (scan (cons q (scan results
                                       (list (list (symbol->string value))) ))
                         (cdr quotations) ) )
                  ((pair? value)
                   (scan (cons q (scan results
                                       (list (list (car value))
                                             (list (cdr value)) ) ))
                         (cdr quotations) ) )
                  (else (static-wrong "unexpected quotation" value)) ) ) )
        results ) ) )

;;; should be tailored to the C compiler.
(define *maximal-fixnum* 16384)
(define *minimal-value* (- *maximal-fixnum*))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooNEW
;;; Combinators. They return a pair of a string (the compiled code in C)
;;; and a list of temporary local variables.

(define (PREDEFINED name)
  (cons (format #f "SCM_~A" (IdScheme->IdC name))
        '() ) )

(define Scheme->C-names-mapping '())  

(define (IdScheme->IdC name)
  (let ((v (assq name Scheme->C-names-mapping)))
    (if (pair? v) (cdr v)
        (let ((str (symbol->string name)))
          (let retry ((Cname (compute-Cname str)))
            (if (Cname-clash? Cname)
                (retry (compute-another-Cname str))
                (begin (set! Scheme->C-names-mapping
                             (cons (cons name Cname) 
                                   Scheme->C-names-mapping ) )
                       Cname ) ) ) ) ) ) )

;;; A new version (not in the book) that systematically converts to uppercase.
(define (IdScheme->IdC name)
  (let ((v (assq name Scheme->C-names-mapping)))
    (if (pair? v) (cdr v)
        ;; convert to uppercase
        (let ((str (convert-string-uppercase (symbol->string name))))
          (let retry ((Cname (compute-Cname str)))
            (if (Cname-clash? Cname)
                (retry (compute-another-Cname str))
                (begin (set! Scheme->C-names-mapping
                             (cons (cons name Cname) 
                                   Scheme->C-names-mapping ) )
                       Cname ) ) ) ) ) ) )

(define (convert-string-uppercase str)
  (list->string (map char-upcase (string->list str))) )

(define (Cname-clash? Cname)
  (let check ((mapping Scheme->C-names-mapping))
    (and (pair? mapping)
         (or (string=? Cname (cdr (car mapping)))
             (check (cdr mapping)) ) ) ) )

(define compute-another-Cname 
  (let ((counter 1))
    (lambda (str)
      (set! counter (+ 1 counter))
      (compute-Cname (format #f "x~A~A" counter str)) ) ) )

(define (compute-Cname str)
  (let ((n (string-length str))
        (result '()) )
    (do ((i 0 (+ 1 i)))
        ((= i n)
         (list->string (reverse result)) )
      (set! result 
            (append
             (let ((char (string-ref str i)))
               (case char
                 ((#\- #\/ #\*) '(#\_))
                 ((#\?) '(#\p))
                 ((#\!) '(#\i))
                 ((#\<) '(#\l))
                 ((#\>) '(#\g))
                 ((#\=) '(#\e))
                 ((#\:) '())
                 (else (list char)) ) )
             result ) ) ) ) )
; (cons (IdScheme->IdC 'foo) Scheme->C-names-mapping)
; (cons (IdScheme->IdC 'foo-bar) Scheme->C-names-mapping)
; (cons (IdScheme->IdC 'foo_bar) Scheme->C-names-mapping)
; (cons (IdScheme->IdC 'set!-f*/*-<b:ar>?) Scheme->C-names-mapping)
; (cons (IdScheme->IdC 'set!-f*/*-<bar>?) Scheme->C-names-mapping)
; Ne traite pas 1+

(define (SHALLOW-ARGUMENT-REF j)
  (cons (format #f "( /* SHALLOW-ARGUMENT-REF */
SCM_The_Environment->frame.value[~A])" (- j 1))
        '() ) )

(define (DEEP-ARGUMENT-REF i j)
  (cons (format #f "( /* DEEP-ARGUMENT-REF */
SCM_The_Environment~A->frame.value[~A])" 
                (let next ((i i))
                  (if (= i 0)
                      ""
                      (format #f "->frame.next~A" (next (- i 1))) ) )
                (- j 1) )
        '() ) )

(define (SHALLOW-ARGUMENT-SET! j m)
  (let ((v (gensym6f "V"))
        (mcode (car m))
        (mvars (cdr m)) )
    (cons (format #f "(/* SHALLOW-ARGUMENT-SET! */
~A=~A,
SCM_The_Environment->frame.value[~A]=~A)"
                  v mcode
                  (- j 1) v )
          (cons v mvars) ) ) )

(define (DEEP-ARGUMENT-SET! i j m)
  (let ((v (gensym6f "V"))
        (mcode (car m))
        (mvars (cdr m)) )
    (cons (format #f "( /* DEEP-ARGUMENT-SET! */
~A=~A,
SCM_The_Environment~A->frame.value[~A]=~A)" 
                  v mcode
                  (let next ((i i))
                    (if (= i 0)
                        ""
                        (format #f "->frame.next~A" (next (- i 1))) ) )
                  (- j 1) v )
          (cons v mvars) ) ) )

(define (GLOBAL-REF i)
  (let ((name (let lookup ((sg.current sg.current))
                (when (pair? sg.current)
                  (if (= i (cddr (car sg.current)))
                      (car (car sg.current))
                      (lookup (cdr sg.current)) ) ) )))
    (PREDEFINED name) ) )

(define (CHECKED-GLOBAL-REF i)
  (let* ((g (GLOBAL-REF i))
         (gcode (car g))
         (gvars (cdr g)) )
    (cons (format #f "( /* CHECKED-GLOBAL-REF */
(~A==SCM_undef)
? SCM_error(100)
: ~A )"
                  gcode
                  gcode )
          gvars ) ) )
;;; Exo: add a C comment with the name of the global variable ?

(define (GLOBAL-SET! i m)
  (let* ((v (gensym6f "V"))
         (g (GLOBAL-REF i))
         (gcode (car g))
         (gvars (cdr g))
         (mcode (car m))
         (mvars (cdr m)) )
    (cons (format #f "( /* GLOBAL-SET! */
~A=~A,
~A=~A)" 
                  v mcode
                  gcode v )
          (cons v (append gvars mvars)) ) ) )

(define (CONSTANT value)
  (let ((qcode (adjoin-quotation value)))
  (cons (format #f "( /* CONSTANT */
(SCM)~A)" qcode )
        '() ) ) )

(define (adjoin-quotation value)
  (let ((q (gensym6f "Q")))
    (set! *quotations* (cons (list value q) *quotations*))
    q ) )
;;; Exo: coalesce constants

(define (ALTERNATIVE m1 m2 m3)
  (let ((m1code (car m1))
        (m1vars (cdr m1))
        (m2code (car m2))
        (m2vars (cdr m2))
        (m3code (car m3))
        (m3vars (cdr m3)) )
    (cons (format #f "( /* IF */
(~A != SCM_false)
? ~A
: ~A)" 
                  m1code m2code m3code )
          (append m1vars m2vars m3vars) ) ) )

(define (SEQUENCE m m+)
  (let ((mcode (car m))
        (mvars (cdr m))
        (m+code (car m+))
        (m+vars (cdr m+)) )
    (cons (format #f "( /* BEGIN */
~A,
~A)"
                  mcode m+code )
          (append mvars m+vars) ) ) )

(define (TR-FIX-LET m* m+)
  (let ((r (gensym6f "R"))
        (m*code (car m*))
        (m*vars (cdr m*))
        (m+code (car m+))
        (m+vars (cdr m+)) )
    (cons (format #f "( /* LET */
~A=~A,
~A->frame.next=SCM_The_Environment,
SCM_The_Environment=~A,
~A)" 
                  r m*code 
                  r 
                  r
                  m+code )
          (cons r (append m*vars m+vars)) ) ) )

(define (FIX-LET m* m+)
  (let ((r (gensym6f "R"))
        (s (gensym6f "S"))
        (v (gensym6f "V"))
        (m*code (car m*))
        (m*vars (cdr m*))
        (m+code (car m+))
        (m+vars (cdr m+)) )
    (cons (format #f "( /* FIX-LET */
~A=SCM_The_Environment,
~A=~A,
~A->frame.next=SCM_The_Environment,
SCM_The_Environment=~A,
~A=~A,
SCM_The_Environment=~A,
~A)" 
                  s
                  r m*code 
                  r 
                  r
                  v m+code 
                  s
                  v )
          (cons r (cons s (cons v (append m*vars m+vars)))) ) ) )

(define (TR-NARY-LET m* m+ arity size-2)
  (let ((r (gensym6f "R"))
        (m*code (car m*))
        (m*vars (cdr m*))
        (m+code (car m+))
        (m+vars (cdr m+)) )
    (cons (format #f "( /* TR-NARY-LET */
~A=~A,
~A->frame.next=SCM_The_Environment,
SCM_The_Environment=~A,
SCM_listify(~A,~A),
~A)" 
                  r m*code 
                  r 
                  r
                  r arity
                  m+code )
          (cons r (append m*vars m+vars)) ) ) )

(define (NARY-LET m* m+ arity size-2)
  (let ((r (gensym6f "R"))
        (s (gensym6f "S"))
        (v (gensym6f "V"))
        (m*code (car m*))
        (m*vars (cdr m*))
        (m+code (car m+))
        (m+vars (cdr m+)) )
    (cons (format #f "( /* NARY-LET */
~A=SCM_The_Environment,
~A=~A,
~A->frame.next=SCM_The_Environment,
SCM_listify(~A,~A),
SCM_The_Environment=~A,
~A=~A,
SCM_The_Environment=~A,
~A)" 
                  s
                  r m*code 
                  r 
                  r arity
                  r
                  v m+code 
                  s
                  v )
          (cons r (cons s (cons v (append m*vars m+vars)))) ) ) )

(define (CALL0 address)
  (cons (format #f "(~A)" (address))
        '() ) )

(define (CALL1 address m1)
  (let ((v1 (gensym6f "V"))
        (m1code (car m1))
        (m1vars (cdr m1)) )
    (cons (format #f "( /* CALL1 */
~A=~A,
~A)"
                  v1 m1code
                  (address v1) )
          (cons v1 m1vars) ) ) )

(define (CALL2 address m1 m2)
  (let ((v1 (gensym6f "V"))
        (v2 (gensym6f "V"))
        (m1code (car m1))
        (m1vars (cdr m1))
        (m2code (car m2))
        (m2vars (cdr m2)) )
    (cons (format #f "( /* CALL2 */
~A=~A,
~A=~A,
~A)"
                  v1 m1code
                  v2 m2code
                  (address v1 v2) )
          (cons v1 (cons v2 (append m1vars m2vars))) ) ) )

(define (CALL3 address m1 m2 m3)
  (let ((v1 (gensym6f "V"))
        (v2 (gensym6f "V"))
        (v3 (gensym6f "V"))
        (m1code (car m1))
        (m1vars (cdr m1))
        (m2code (car m2))
        (m2vars (cdr m2))
        (m3code (car m3))
        (m3vars (cdr m3)) )
    (cons (format #f "( /* CALL3*/
~A=~A,
~A=~A,
~A=~A,
~A)"
                  v1 m1code
                  v2 m2code
                  v3 m3code
                  (address v1 v2 v3) )
          (cons v1 (cons v2 (cons v3 (append m1vars m2vars m3vars)))) ) ) )

(define (local-temporaries vars)
  (if (pair? vars)
      (format #f "SCM ~A~A ;" 
              (car vars)
              (apply string-append 
                     (map (lambda (var) (format #f ", ~A" var))
                          (cdr vars) ) ) )
      "" ) )

(define (FIX-CLOSURE m+ arity)
  (let* ((f (adjoin-closure arity m+ #f))
         (fcode (car f))
         (fvars (cdr f)) )
    (cons (format #f "(SCM_make_function(~A,SCM_The_Environment))"
                  fcode ) 
          fvars ) ) )

(define (adjoin-closure arity m+ nary?)
  (let ((f (gensym6f "Function"))
        (v* (gensym6f "Frame"))
        (r (gensym6f "R"))
        (m+code (car m+))
        (m+vars (cdr m+)) )
    (set! *closures*
          (cons (cons f (format #f "
SCM
~A(SCM ~A, SCM ~A) { ~A
  if ( !(~A->frame.size~A(~A+2)) ) { SCM_error(11);
  } else { ~A->frame.next=~A;
           SCM_The_Environment=~A;  ~A
  }
  return ~A;
}
"                               f v* r (local-temporaries m+vars)
                                v* (if nary? ">=" "==") arity
                                v* r
                                v* (if nary?
                                       (format #f "~%SCM_listify(~A,~A);"
                                               v* arity )
                                       "" )
                                m+code ))
                *closures* ) )
    (cons f
          ;; do not return f since it will be made global to the C file
          (cons v* (cons r m+vars)) ) ) )

(define (NARY-CLOSURE m+ arity)
  (let* ((f (adjoin-closure arity m+ #t))
         (fcode (car f))
         (fvars (cdr f)) )
    (cons (format #f "(SCM_make_function(~A,SCM_The_Environment))"
                  fcode )
          fvars ) ) )

(define (TR-REGULAR-CALL m m*)
  (let ((f (gensym6f "F"))
        (v* (gensym6f "Frame"))
        (mcode (car m))
        (mvars (cdr m))
        (m*code (car m*))
        (m*vars (cdr m*)) )
    (cons (format #f "( /* TR-REGULAR-CALL */
~A=~A,
~A=~A,
SCM_call(~A,~A) )"
                  f mcode
                  v* m*code
                  f v* )
          (cons f (cons v* (append mvars m*vars))) ) ) )

(define (REGULAR-CALL m m*)
  (let ((f (gensym6f "F"))
        (v* (gensym6f "Frame"))
        (r (gensym6f "S"))
        (v (gensym6f "V"))
        (mcode (car m))
        (mvars (cdr m))
        (m*code (car m*))
        (m*vars (cdr m*)) )
    (cons (format #f "( /* REGULAR-CALL */
~A=SCM_The_Environment,
~A=~A,
~A=~A,
~A=SCM_call(~A,~A),
SCM_The_Environment=~A,
~A )"
                  r
                  f mcode
                  v* m*code
                  v f v* 
                  r
                  v )
          (cons f (cons v* (cons r (cons v (append mvars m*vars))))) ) ) )

(define (STORE-ARGUMENT m m* rank)
  (let ((v (gensym6f "V"))
        (v* (gensym6f "Frame"))
        (mcode (car m))
        (mvars (cdr m))
        (m*code (car m*))
        (m*vars (cdr m*)) )
    (cons (format #f "( /* STORE-ARGUMENT */
~A=~A,
~A=~A,
~A->frame.value[~A]=~A,
~A )"
                  v mcode
                  v* m*code
                  v* (- rank 1) v
                  v* )
           (cons v (cons v* (append mvars m*vars))) ) ) )

(define (ALLOCATE-FRAME size)
  (cons (format #f "(SCM_make_frame(~A))" size)
        '() ) )
;;; EN fait un de trop puisque le champ next est deja alloue ailleurs
;;; mais cela change toutes les verifications ailleurs.            TEMP

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The threaded interpreter.
;;; E is the expression to evaluate
;;; SR is the representation of the local lexical environment
;;; TR is a boolean that indicates if E is a terminal call (also means whether
;;; the *env* register should be restored or not).

(define (meaning e sr tr)
  (if (atom? e)
      (if (symbol? e) (meaning-reference e sr tr)
                      (meaning-quotation e sr tr) )
      (case (car e)
        ((quote)  (meaning-quotation (cadr e) sr tr))
        ((lambda) (meaning-abstraction (cadr e) (cddr e) sr tr))
        ((if)     (meaning-alternative (cadr e) (caddr e) (cadddr e) sr tr))
        ((begin)  (meaning-sequence (cdr e) sr tr))
        ((set!)   (meaning-assignment (cadr e) (caddr e) sr tr))
        (else     (meaning-application (car e) (cdr e) sr tr)) ) ) )

(define (meaning-reference n sr tr)
  (let ((kind (compute-kind sr n)))
    (if kind
        (case (car kind)
          ((local)
           (let ((i (cadr kind))
                 (j (cddr kind)) )
             (if (= i 0)
                 (SHALLOW-ARGUMENT-REF j)
                 (DEEP-ARGUMENT-REF i j) ) ) )
          ((global)
           (let ((i (cdr kind)))
             (CHECKED-GLOBAL-REF i) ) )
          ((predefined)
           (let* ((i (cdr kind)))
             (PREDEFINED n) ) ) )
        (static-wrong "No such variable" n) ) ) )

(define (meaning-quotation v sr tr)
  (CONSTANT v) )

(define (meaning-alternative e1 e2 e3 sr tr)
  (let ((m1 (meaning e1 sr #f))
        (m2 (meaning e2 sr tr))
        (m3 (meaning e3 sr tr)) )
    (ALTERNATIVE m1 m2 m3) ) )

(define (meaning-assignment n e sr tr) 
  (let ((m (meaning e sr #f))
        (kind (compute-kind sr n)) )
    (if kind
        (case (car kind)
          ((local)
           (let ((i (cadr kind))
                 (j (cddr kind)) )
             (if (= i 0)
                 (SHALLOW-ARGUMENT-SET! j m)
                 (DEEP-ARGUMENT-SET! i j m) ) ) )
          ((global)
           (let ((i (cdr kind)))
             (GLOBAL-SET! i m) ) )
          ((predefined)
           (static-wrong "Immutable predefined variable" n) ) )
        (static-wrong "No such variable" n) ) ) )

(define (meaning-sequence e+ sr tr)
  (if (pair? e+)
      (if (pair? (cdr e+))
          (meaning*-multiple-sequence (car e+) (cdr e+) sr tr)
          (meaning*-single-sequence (car e+) sr tr) )
      (static-wrong "Illegal syntax: (begin)") ) )

(define (meaning*-single-sequence e sr tr) 
  (meaning e sr tr) )

(define (meaning*-multiple-sequence e e+ sr tr)
  (let ((m1 (meaning e sr #f))          ; restore environment!
        (m+ (meaning-sequence e+ sr tr)) )
    (SEQUENCE m1 m+) ) )

(define (meaning-abstraction nn* e+ sr tr)
  (let parse ((n* nn*)
              (regular '()) )
    (cond
     ((pair? n*) (parse (cdr n*) (cons (car n*) regular)))
     ((null? n*) (meaning-fix-abstraction nn* e+ sr tr))
     (else       (meaning-dotted-abstraction 
                  (reverse regular) n* e+ sr tr )) ) ) )

(define (meaning-fix-abstraction n* e+ sr tr)
  (let* ((arity (length n*))
         (sr2 (sr-extend* sr n*))
         (m+ (meaning-sequence e+ sr2 #t)) )
    (FIX-CLOSURE m+ arity) ) )

(define (meaning-dotted-abstraction n* n e+ sr tr)
  (let* ((arity (length n*))
         (sr2 (sr-extend* sr (append n* (list n))))
         (m+ (meaning-sequence e+ sr2 #t)) )
    (NARY-CLOSURE m+ arity) ) )

;;; Application meaning.

(define (meaning-application e e* sr tr)
  (cond ((and (symbol? e)
              (let ((kind (compute-kind sr e)))
                (and (pair? kind)
                     (eq? 'predefined (car kind))
                     (let ((desc (get-description e)))
                       (and desc
                            (eq? 'function (car desc))
                            (= (length (cddr desc)) (length e*)) ) ) ) ) )
         (meaning-primitive-application e e* sr tr) )
        ((and (pair? e)
              (eq? 'lambda (car e)) )
         (meaning-closed-application e e* sr tr) )
        (else (meaning-regular-application e e* sr tr)) ) )

;;; Parse the variable list to check the arity and detect wether the
;;; abstraction is dotted or not.

(define (meaning-closed-application e ee* sr tr)
  (let ((nn* (cadr e)))
    (let parse ((n* nn*)
                (e* ee*)
                (regular '()) )
      (cond
       ((pair? n*) 
        (if (pair? e*)
            (parse (cdr n*) (cdr e*) (cons (car n*) regular))
            (static-wrong "Too less arguments" e ee*) ) )
       ((null? n*)
        (if (null? e*)
            (meaning-fix-closed-application 
             nn* (cddr e) ee* sr tr )
            (static-wrong "Too much arguments" e ee*) ) )
       (else (meaning-dotted-closed-application 
              (reverse regular) n* (cddr e) ee* sr tr )) ) ) ) )

(define (meaning-fix-closed-application n* body e* sr tr)
  (let* ((size (+ 1 (length e*) 1))
         (m* (meaning* e* sr size #f))  ; restore environment!
         (sr2 (sr-extend* sr n*))
         (m+ (meaning-sequence body sr2 tr)) )
    (if tr (TR-FIX-LET m* m+) 
        (FIX-LET m* m+) ) ) )

(define (meaning-dotted-closed-application n* n body e* sr tr)
  (let* ((size-2 (length e*))
         (size (+ 1 size-2 1))
         (arity (length n*))
         (m* (meaning* e* sr size #f))  ; restore environment!
         (sr2 (sr-extend* sr (append n* (list n))))
         (m+ (meaning-sequence body sr2 tr)) )
    (if tr (TR-NARY-LET m* m+ arity size-2) 
        (NARY-LET m* m+ arity size-2) ) ) )

;;; Handles a call to a predefined primitive. The arity is already checked.
;;; The optimization is to avoid the allocation of the activation frame.
;;; These primitives never change the *env* register nor have control effect.

(define (meaning-primitive-application e e* sr tr)
  (let* ((desc (get-description e))
         ;; desc = (function <address> . <variables-list>)
         (address (cadr desc))
         (size (length e*)) )
    (case size
      ((0) (CALL0 address))
      ((1) 
       (let ((m1 (meaning (car e*) sr tr)))
         (CALL1 address m1) ) )
      ((2) 
       (let ((m1 (meaning (car e*) sr #f))
             (m2 (meaning (cadr e*) sr tr)) )
         (CALL2 address m1 m2) ) )
      ((3) 
       (let ((m1 (meaning (car e*) sr #f))
             (m2 (meaning (cadr e*) sr #f))
             (m3 (meaning (caddr e*) sr tr)) )
         (CALL3 address m1 m2 m3) ) )
      (else (meaning-regular-application e e* sr tr)) ) ) )

;;; In a regular application, the invocation protocol is to call the
;;; function with an activation frame and a continuation: (f v* k).

(define (meaning-regular-application e e* sr tr)
  (let* ((m (meaning e sr #f))
         (args-number (length e*))
         (size (+ 1 args-number 1)) 
         (m* (meaning* e* sr size #t)) )
    (if tr (TR-REGULAR-CALL m m*) (REGULAR-CALL m m*)) ) )

(define (meaning* e* sr size tr)
  (if (pair? e*)
      (meaning-some-arguments (car e*) (cdr e*) sr size tr)
      (meaning-no-argument sr size tr) ) )

(define (meaning-some-arguments e e* sr size tr)
  (let ((m (meaning e sr #f))
        (m* (meaning* e* sr size tr))
        (rank (- size (+ 1 (length e*) 1))) )
    (STORE-ARGUMENT m m* rank) ) )

(define (meaning-no-argument sr size tr)
  (ALLOCATE-FRAME size) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Global environment initializers.

;;; Definitial allows to redefine immutable global variables. Useful
;;; when debugging interactively.

(define-syntax definitial
  (syntax-rules ()
    ((definitial name value)
     (begin (sg.init-extend! 'name)
            'name ) ) ) )

(define-syntax defprimitive0
  (syntax-rules ()
    ((defprimitive0 name value)
     (begin
       (description-extend! 'name `(function ,value))
       (definitial name 'void) ) ) ) )
  
(define-syntax defprimitive1
  (syntax-rules ()
    ((defprimitive1 name value)
     (begin
       (description-extend! 'name `(function ,value a))
       (definitial name 'void) ) ) ) )
  
(define-syntax defprimitive2
  (syntax-rules ()
    ((defprimitive2 name value)
     (begin 
       (description-extend! 'name `(function ,value a b))
       (definitial name 'void) ) ) ) )

(define-syntax defprimitive3
  (syntax-rules ()
    ((defprimitive3 name value)
     (begin 
       (description-extend! 'name `(function ,value a b c))
       (definitial name 'void) ) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooNEW
;;; Invocation generators

(define (zeroadic-call image)
  (lambda ()
    (format #f "~A" image) ) )

(define (unary-call start end)
  (lambda (a)
    (format #f "~A~A~A" start a end) ) )

(define (binary-call start middle end)
  (lambda (a b)
    (format #f "~A~A~A~A~A" start a middle b end) ) )

(define (ternary-call start middle1 middle2 end)
  (lambda (a b c)
    (format #f "~A~A~A~A~A~A~A" start a middle1 b middle2 c end) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooNEW
;;; Initialization of the predefined global environment.

(definitial t #t)
(definitial f #f)
(definitial nil '())

(defprimitive1 pair? (unary-call "SCM_PairP(" ")"))
(defprimitive1 null? (unary-call "SCM_NullP(" ")"))
(defprimitive1 integer? (unary-call "SCM_FixnumP(" ")"))
(defprimitive1 symbol? (unary-call "SCM_SymbolP(" ")"))
(defprimitive1 string? (unary-call "SCM_StringP(" ")"))
(defprimitive1 char? (unary-call "SCM_CharP(" ")"))
(defprimitive1 eof-object? (unary-call "SCM_EofP(" ")"))
(defprimitive1 procedure? (unary-call "SCM_ProcedureP(" ")"))
(defprimitive2 eq? (binary-call "SCM_EqP(" "," ")"))
(defprimitive2 cons (binary-call "SCM_Cons(" "," ")"))
(defprimitive1 car (unary-call "SCM_Car(" ")"))
(defprimitive1 cdr (unary-call "SCM_Cdr(" ")"))
(defprimitive2 set-car! (binary-call "SCM_Set_Car(" "," ")"))
(defprimitive2 set-cdr! (binary-call "SCM_Set_Cdr(" "," ")"))
(definitial list tobecodedspecially)
(defprimitive2 + (binary-call "SCM_Plus(" "," ")"))
(defprimitive2 - (binary-call "SCM_Minus(" "," ")"))
(defprimitive2 * (binary-call "SCM_Times(" "," ")"))
(defprimitive2 quotient (binary-call "SCM_Quotient(" "," ")"))
(defprimitive2 remainder (binary-call "SCM_Remainder(" "," ")"))
(defprimitive2 > (binary-call "SCM_GtP(" "," ")"))
(defprimitive2 < (binary-call "SCM_LtP(" "," ")"))
(defprimitive2 >= (binary-call "SCM_GeP(" "," ")"))
(defprimitive2 <= (binary-call "SCM_LeP(" "," ")"))
(defprimitive2 = (binary-call "SCM_EqnP(" "," ")"))
(defprimitive1 display (unary-call "SCM_Basic_Write(" ")")) ; TEMP
(defprimitive1 char->integer (unary-call "SCM_Char2Integer(" ")"))
(defprimitive1 integer->char (unary-call "SCM_Integer2Char(" ")"))
(defprimitive1 symbol->string (unary-call "SCM_Symbol2String(" ")"))
(defprimitive1 make-symbol (unary-call "SCM_Make_Symbol(" ")"))
(defprimitive1 exit (unary-call "SCM_Exit(" ")"))
(defprimitive1 get-universal-time (zeroadic-call "SCM_Get_Universal_Time()"))
(defprimitive2 float:+ (binary-call "SCM_Float_Plus(" "," ")"))
(defprimitive2 float:- (binary-call "SCM_Float_Minus(" "," ")"))
(defprimitive2 float:* (binary-call "SCM_Float_Multiply(" "," ")"))
(defprimitive2 float:/ (binary-call "SCM_Float_Divide(" "," ")"))
(defprimitive2 float:> (binary-call "SCM_Float_GreaterP(" "," ")"))
(defprimitive2 float:< (binary-call "SCM_Float_LessP(" "," ")"))
(defprimitive2 float:= (binary-call "SCM_Float_EqnP(" "," ")"))
(defprimitive1 read-char (unary-call "SCM_Read_Char(" ")"))
(defprimitive1 peek-char (unary-call "SCM_Peek_Char(" ")"))
(defprimitive1 string-length (unary-call "SCM_String_Length(" ")"))
(defprimitive2 string-ref (binary-call "SCM_String_Ref(" "," ")"))
(defprimitive3 string-set! (ternary-call "SCM_String_Set(" "," "," ")"))
(defprimitive1 not (unary-call "SCM_Not(" ")"))
(defprimitive1 call/cc (unary-call "SCM_Call_CC(" ")"))
(definitial apply tobecodedspecially)

;;; Debug 
(defprimitive0 show-the-environment 
  (zeroadic-call "SCM_Basic_Write(SCM_The_Environment)") )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooNEW
;;; Testing

(define (scheme)
  (interpreter 
   "Expression to compile? "  
   "Compilation " 
   #t
   (lambda (read print error)
     (set! wrong error)
     (set! static-wrong error)
     (lambda ()
       (compile->C stdout-port (read))
       (print 'done) ) ) ) )

(define (compile-file infile outfile)
  (call-with-input-file infile
    (lambda (in)
      ;; compile a single expression   TEMP
      (let ((e (read in)))
        (call-with-output-file outfile
          (lambda (out)
            (compile->C out e) ) ) ) ) ) )

(define (test-scheme file)
  (call/cc
   (lambda (finish)
     (define native-read read)
     (define expected-result 'wait)
     (call-with-input-file file
       (lambda (in)
         (engine-tester
          (lambda ()
            (let ((e (read in)))
              (if (eof-object? e) (finish e) e) ) )
          (lambda () expected-result)
          equal?
          (lambda (status expected v)
            (case status
              ((error-occurred)
               (display " OK OK") (newline) #t )
              ((uninteresting-result correct-result)
               (display " OK") (newline) #t )
              ((expected-error unexpected-error incorrect-result)
               (set! *the-result* v)    ; DEBUG
               (error 'chap6f "value expected: ~A." expected) 
               #f )
              (else (error 'chap6f "No such status ~A." status)
                    #f ) ) )
          (lambda (read check error)
            (set! wrong 
                  (lambda args
                    (format #t "~%Fatal error, test aborted!~%")
                    (apply error args) ) )
            (set! static-wrong wrong)
            (lambda ()
              (define (skip-read)
                (let ((e (read)))
                  (if (member e *tests-to-skip*)
                      (begin (read) ; skip the associated result
                             (skip-read) )
                      e ) ) )
              (let* ((test (skip-read))
                     (e `(display ,test))
                     (status 0) )
                (set! expected-result (read))
                (format stderr-port "~%===> Expression:~%")
                (pp e stderr-port)
                (format stderr-port "~%===> Compiling towards C...")
                (flush-buffer stderr-port)
                (call-with-output-file "/tmp/chap6f.c"
                  (lambda (out) (compile->C out e)) )
                (format stderr-port "~%===> C compilation...")
                (flush-buffer stderr-port)
                (set! status (system *compile-format*))
                (unless (= status 0) (static-wrong "Cannot compile"))
                (format stderr-port "~%===> Run...~%")
                (flush-buffer stderr-port)
                (set! status (system "/tmp/a.out > /tmp/chap6f.result"))
                (unless (= status 0) (static-wrong "Cannot run"))
                (system "cat /tmp/chap6f.result")
                (format stderr-port "~%===> Check result...")
                (flush-buffer stderr-port)
                (if (= status 0)
                    (if (eq? expected-result '---)
                        ;; Don't read /tmp/chap6f.result if needless
                        (check '---)
                        (check (call-with-input-file 
                                   "/tmp/chap6f.result" native-read )) )
                    (check '***) ) ) ) ) ) ) ) ) ) ) 

(define *compile-format*
  "cd /tmp ; gcc -gg -I${HOME}/DEA/book/src/c chap6f.c ${HOME}/DEA/book/o/${HOSTTYPE}/rt.o" )

;;; Skip these tests
(define *tests-to-skip*
  '( xyzzy
     (set! xyzzy 3)
     ) )

;;; Bigloo's gensyms are not legal identifiers for C, so redefine
;;; gensym for that file only.

(define gensym6f
  (let ((counter 99))
    (lambda args
      (set! counter (+ counter 1))
      (string->symbol
       (string-append
        (if (pair? args) (car args) "G")
        (number->string counter) ) ) ) ) )

;;; DEBUG
(define *the-result* #f)

;;; PERSONAL NOTES
;;; faire extent dynamique dans rt.c

;;; changer tous les offsets dans les interpretes precedents

;;; end of chap6f.scm
