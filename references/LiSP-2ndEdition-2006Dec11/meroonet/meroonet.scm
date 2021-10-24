;;; $Id: meroonet.scm,v 1.21 2006/12/02 18:33:32 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; This is a restricted version of Meroon intended to be only
;;; pedagogical. The main differences with Meroon are:
;;; -- Less features than in Meroon V3 (no option in Meroonet macros).
;;; -- Less Meta-Object features. The autodescription of Meroonet 
;;;    is still there but there is no causal link. It is not possible to
;;;    add new types of Field (well it is not easy).
;;; -- No user-friendly error notification (most verifications are not
;;;    done, this will provoke weird behavior if ever tried). The sole 
;;;    errors that are caught invoke the meroonet-error function which is
;;;    intentionally not defined (this probably invokes the underlying
;;;    error system).
;;; -- No effort for efficiency apart the access to Meroonet instances 
;;;    without Poly-Fields.
;;; -- No separation between macroexpansion-time, compile-time,
;;;    load-time. Every defining form of Meroonet is expected to be
;;;    immediately executed, one by one, at toplevel.  
;;; -- No predefined library such as clone, show, show-unveiled etc.
;;; The documentation of Meroon V2 is entirely appropriate except for
;;; the points stated above. Normal usage of Meroon should not see a
;;; difference. Consult the `Notice' companion file for more details.

;;; Since the previous list of restrictions may seem sad, here are 
;;; the features that still appear in Meroonet:
;;; ++ Some efforts for efficiency in some places (fast access to Meroonet
;;;    instances not using poly-fields, fast (non-consing) allocators for 
;;;    small instances). Meroonet is in fact 20% faster than Meroon V2.
;;; ++ unrestricted inheritance in presence of poly-fields.
;;; ++ autodescription of Meroonet since classes are Meroonet objects.

;;; These restrictions make this file shorter and easier to
;;; understand and thus can appear in an annex of a book I am
;;; preparing.  I nevertheless use the same physical representation
;;; for instances than in Meroon but simplified the innards of generic
;;; functions. Another point of interest is that the bootstrap does not
;;; use any Meroonet macros.

;;; This file uses and thus requires a macro called
;;; `define-meroonet-macro' to define macros. See the Imakefile
;;; companion file for examples. To use Meroonet, just load this file
;;;            (load "meroonet.scm")
;;; Only a few errors are detected by Meroonet. In that case the
;;; meroonet-error function is invoked on a message indicating the
;;; nature of the error and sometimes additional hint(s).

;;;==================================================== Legalistic introduction
;;; This program is distributed in the hope that it will be useful.
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted, so long as the following
;;; conditions are met:
;;;      o credit to the authors is acknowledged following current
;;;        academic behaviour
;;;      o no fees or compensation are charged for use, copies, or
;;;        access to this software
;;;      o this copyright notice is included intact.
;;; This software is made available AS IS, and no warranty is made about
;;; the software or its performance.

;;; Bug descriptions, use reports, comments or suggestions are welcome.
;;; Send them to
;;;   Christian Queinnec             or to:  Christian Queinnec
;;;   <queinnec@polytechnique.fr>            <Christian.Queinnec@inria.fr>
;;;   Laboratoire d'Informatique de l'X      INRIA -- Rocquencourt
;;;   Ecole Polytechnique                    Domaine de Voluceau, BP 105
;;;   91128 Palaiseau                        78153 Le Chesnay Cedex
;;;   France                                 France


;;;==================================================== Additional notes
;;; Functions with a `generate-' prefix generates code that will be
;;; part of macro expansion, functions with a `make-' prefix actually
;;; return objects (generally closures). 
;;; The word `index' is used to specify a particular value in a
;;; Poly-Field, the word `offset' is used to speak of the
;;; representation of instances.

;;;==================================================== Housekeeping
;;; Meroonet restricts the total number of possible different classes.
;;; Moreover this number cannot be dynamically modified.
(define *maximal-number-of-classes* 100)

;;; This vector tabulates classes. Any class can be retrieved by its index.
(define *classes* (make-vector *maximal-number-of-classes* #f))

;;; Convert a number (taken as an index) into a class. 
;;; Restriction: Do not check whether the index is correct.
(define (number->class n)
  (vector-ref *classes* n) )

;;; The index in *classes* designating the first empty slot. It also
;;; represents the total number of defined classes.
(define *class-number* 0)

;;; Converts a symbol into a class with such a name, returns the newest one.
;;; Restriction: Return #f if not found instead of complaining.
(define (->Class name)
  (let scan ((index (- *class-number* 1)))
    (and (>= index 0)
         (let ((c (vector-ref *classes* index)))
           (if (eq? name (Class-name c))
               c (scan (- index 1)) ) ) ) ) )

;;; Meroonet does not restrict the number of generic functions that
;;; can be defined. They are all kept in a list. 
(define *generics* (list))

;;; Converts a symbol to a generic instance with such a name. Returns
;;; the first one ie the newest one.
(define (->Generic name)
  (let lookup ((l *generics*))
    (if (pair? l)
        (if (eq? name (Generic-name (car l)))
            (car l)
            (lookup (cdr l)) )
        #f ) ) )

;;; This variable holds the last class defined at macroexpand-time. It
;;; avoids to rebuild the class at evaluation-time if immediately
;;; after as under a toplevel loop. It is useful for separate
;;; compilation (see the define-class macro).
(define *last-defined-class* #f)

;;;==================================================== Representation
;;; Instances are represented by regular vectors. The first slot of
;;; the vector contains the number of the class the instance is direct
;;; member of. Indexed slots are represented by a succession of values
;;; prefixed by the number of such values. For example:
;;;     (define-class Point Object (x y))
;;;     (define-class Polygon Point ((* point)(= z)))
;;; Polygon has instances represented by:
;;;     +--------------+
;;;     + class-number +    representing the Polygon class
;;;     +--------------+
;;;     | value of X   |    <-- *starting-offset* 
;;;     | value of Y   |
;;;     +  # of points +
;;;     | point[0]     |
;;;     | point[1]     |
;;;                ...
;;;     | point[n]     |
;;;     | value of Z   |
;;;     +--------------+
;;; The first field of an object starts with this offset.
(define *starting-offset* 1)

;;; Returns the class of a Meroonet instance. Assume that O is a
;;; Meroonet object.
(define (object->class o)
  (vector-ref *classes* (vector-ref o 0)) )

;;; This predicate is a rough approximate. All Meroonet objects are
;;; recognized but also vector of integers etc. This might be revised
;;; when Scheme defines new types of values.
(define (Object? o)
  (and (vector? o)
       (integer? (vector-ref o 0)) ) )

;;; Take a list of symbols and return a symbol with the concatenated
;;; name.  Useful to create derived names.
(define (symbol-concatenate . names)
  (string->symbol (apply string-append (map symbol->string names))) )

;;;==================================================== predefined accessors
;;; These readers do not check the class of their argument. They are
;;; and should only be used throughout this implementation.
(define (careless-Class-name class)
  (vector-ref class 1) )
(define (careless-Class-number class)
  (vector-ref class 2) )
(define (careless-Class-fields class)
  (vector-ref class 3) )
(define (careless-Class-superclass class)
  (vector-ref class 4) )

(define (careless-Field-name field)
  (vector-ref field 1) )
(define (careless-Field-defining-class-number field)
  (vector-ref field 2) )

;;;==================================================== Initial hierarchy
;;; The initial hierarchy of classes (by hand)

(define Object-class
  (vector 1                             ; it is a class
          'Object                       ; name
          0                             ; class-number
          '()                           ; fields
          #f                            ; no superclass
          '(1 2 3)                      ; subclass-numbers
          ) )

(define Class-class
  (vector 
   1                                    ; it is also a class
   'Class                               ; name
   1                                    ; class-number
   (list                                ; fields
    (vector 4 'name             1)      ; offset= 1
    (vector 4 'number           1)      ; offset= 2
    (vector 4 'fields           1)      ; offset= 3
    (vector 4 'superclass       1)      ; offset= 4
    (vector 4 'subclass-numbers 1) )    ; offset= 5
   Object-class                         ; superclass
   '() ) )

(define Generic-class
  (vector 
   1
   'Generic
   2
   (list
    (vector 4 'name           2)        ; offset= 1
    (vector 4 'default        2)        ; offset= 2
    (vector 4 'dispatch-table 2)        ; offset= 3
    (vector 4 'signature      2) )      ; offset= 4
    Object-class
    '() ) )

(define Field-class
  (vector 
   1
   'Field
   3
   (list
    (vector 4 'name                  3) ; offset= 1
    (vector 4 'defining-class-number 3) ; offset= 2
    )
   Object-class
   '(4 5) ) )

(define Mono-Field-class
  (vector 1
          'Mono-Field
          4
          (careless-Class-fields Field-class)
          Field-class
          '() ) )

(define Poly-Field-class
  (vector 1
          'Poly-Field
          5
          (careless-Class-fields Field-class)
          Field-class
          '() ) )

;;; Install classes
(vector-set! *classes* 0 Object-class)
(vector-set! *classes* 1 Class-class)
(vector-set! *classes* 2 Generic-class)
(vector-set! *classes* 3 Field-class)
(vector-set! *classes* 4 Mono-Field-class)
(vector-set! *classes* 5 Poly-Field-class)

;;; Next free class-number
(set! *class-number* 6)

;;;==================================================== Class definition
;;; Syntax: (define-class <name-of-the-class> <name-of-the-superclass>
;;;             ( <name-of-a-Mono-Field>          |
;;;               (= <name-of-a-Mono-Field>)      |
;;;               (* <name-of-a-Poly-Field>)      
;;;               ... ) )

;;; The macro call is supposed to be correct: it does not check that
;;; name is a correct name, that supername is a correct name that
;;; names a currently existing class. The strategy is still to create
;;; the Class instance and to ask it to generate its equivalent code
;;; as the result of the macro call. Were Class-generate-related-names be
;;; a generic function as in Meroon V3, this would allow to define new
;;; metaclass with alternate expansion.
;;; Restriction: No class options, nor field options.
(define-meroonet-macro (define-class name supername 
                                     own-field-descriptions )
  (set! *last-defined-class* #f)
  (let ((class (register-class name supername 
                               own-field-descriptions )))
    (set! *last-defined-class* class)
    `(begin
       (if (not *last-defined-class*)
         (register-class ',name ',supername 
                         ',own-field-descriptions ) )
      ,(Class-generate-related-names class) ) ) )

;;; Generate from a class all the definitions of the related functions.
;;; As in Meroon V3 the Class instance is stored in a global variable.
(define (Class-generate-related-names class)
  (let* ((name (Class-name class))
         (class-variable-name (symbol-concatenate name '-class))
         (predicate-name (symbol-concatenate name '?))
         (maker-name (symbol-concatenate 'make- name))
         (allocator-name (symbol-concatenate 'allocate- name)) )
    `(begin 
       (define ,class-variable-name (->Class ',name))
       (define ,predicate-name (make-predicate ,class-variable-name))
       (define ,maker-name (make-maker ,class-variable-name))
       (define ,allocator-name (make-allocator ,class-variable-name))
       ,@(map (lambda (field) 
                (Field-generate-related-names field class) )
              (Class-fields class) )
       ',(Class-name class) ) ) )

;;;==================================================== Predicate
;;; Create from a class its appropriate recognizer.
(define (make-predicate class)
  (lambda (o) (and (Object? o)
                   (is-a? o class) )) )

;;; Tests if an object is an instance of a class.
;;; This one is linear but fast since most of the time 
;;; (eq? (object->class o) class). It also assumes (Object? o) and
;;; (Class? class) are true.
(define (is-a? o class)
  (let up ((c (object->class o)))
    (or (eq? class c)
        (let ((sc (careless-Class-superclass c)))
          (and sc (up sc)) ) ) ) )

;;; Check if an object is an instance of a class. If you appreciate a
;;; dangerous life just nullify it!
(define (check-class-membership o class)
  (if (not (is-a? o class))
    (meroonet-error "Wrong class" o class) ) )
    
;;;==================================================== Allocation
;;; Create from a class its appropriate allocator. Sizes must be a
;;; list of positive integers, fields must only be Mono- or Poly-Fields.
;;; Restriction: superfluous sizes are silently ignored.
(define (make-allocator class)
  (let ((fields (Class-fields class)))
    (lambda sizes
      ;; compute the size of the instance to allocate
      (let ((room (let iter ((fields fields)
                             (sizes sizes)
                             (room *starting-offset*) )
                    (if (pair? fields)
                        (cond ((Mono-Field? (car fields))
                               (iter (cdr fields) sizes (+ 1 room)) )
                              ((Poly-Field? (car fields))
                               (iter (cdr fields) (cdr sizes) 
                                     (+ 1 (car sizes) room) ) ) )
                        room ) )))
        (let ((o (make-vector room #f)))
          ;; setup instantiation link and skeleton of the instance
          (vector-set! o 0 (Class-number class))
          (let iter ((fields fields)
                     (sizes sizes)
                     (offset *starting-offset*) )
            (if (pair? fields)
                (cond ((Mono-Field? (car fields)) 
                       (iter (cdr fields) sizes (+ 1 offset)) )
                      ((Poly-Field? (car fields))
                       (vector-set! o offset (car sizes))
                       (iter (cdr fields) (cdr sizes) 
                             (+ 1 (car sizes) offset) ) ) )
                o ) ) ) ) ) ) )           

;;; Create from a class an appropriate maker. The strategy is to create the
;;; vector then to check if it has an appropriate skeleton. Improve
;;; the creation of instances without poly-fields ie do not cons.
;;; Restriction: Do not check if there are too much values given for
;;; initialization, they are ignored.
(define (make-maker class)
  (or (make-fix-maker class)
      (let ((fields (Class-fields class)))
        (lambda parms
          ;; create the instance
          (let ((o (apply vector (Class-number class) parms)))
            ;; check its skeleton
            (let check ((fields fields)
                        (parms parms)
                        (offset *starting-offset*) )
              (if (pair? fields)
                  (cond ((Mono-Field? (car fields)) 
                         (check (cdr fields) 
                                (cdr parms)
                                (+ 1 offset) ) )
                        ((Poly-Field? (car fields))
                         (check (cdr fields) 
                                (list-tail (cdr parms) (car parms))
                                (+ 1 (car parms) offset) ) ) )
                  o ) ) ) ) ) ) )

;;; Create from a class without Poly-Fields a direct maker with a fix
;;; arity: this saves some cons cells.
(define (make-fix-maker class)
  (define (static-size? fields)
    (if (pair? fields)
        (and (Mono-Field? (car fields))
             (static-size? (cdr fields)) )
        #t ) )
  (let ((fields (Class-fields class)))
    (and (static-size? fields)
         (let ((size (length fields))
               (cn (Class-number class)) )
           (case size
             ((0)  (lambda () (vector cn)))
             ((1)  (lambda (a) (vector cn a)))
             ((2)  (lambda (a b) (vector cn a b)))
             ((3)  (lambda (a b c) (vector cn a b c)))
             ((4)  (lambda (a b c d) (vector cn a b c d)))
             ((5)  (lambda (a b c d e) (vector cn a b c d e)))
             ((6)  (lambda (a b c d e f) (vector cn a b c d e f)))
             ((7)  (lambda (a b c d e f g) (vector cn a b c d e f g)))
             (else #f) ) ) ) ) )

;;;==================================================== Access
;;; Generate from a field all the definitions of the related accessors.
;;; An instance of Field refers to the class that introduced it. Only the 
;;; number of the class is given to avoid circular objects.

;;; Names are constructed with the current class rather than the
;;; defining class, that is why the class is also given to this
;;; function. The lengther is also generated in case of a Poly-Field.
(define (Field-generate-related-names field class)
  (let* ((fname (careless-Field-name field))
         (cname (Class-name class))
         (cname-variable (symbol-concatenate cname '-class))
         (reader-name (symbol-concatenate cname '- fname))
         (writer-name (symbol-concatenate 'set- cname '- fname '!)) )
    `(begin 
       (define ,reader-name 
         (make-reader 
          (retrieve-named-field ,cname-variable ',fname) ) )
       (define ,writer-name 
         (make-writer 
          (retrieve-named-field ,cname-variable ',fname) ) )
       ,@(if (Poly-Field? field)
             `((define ,(symbol-concatenate cname '- fname '-length)
                 (make-lengther
                  (retrieve-named-field ,cname-variable ',fname) ) ))
             '() ) ) ) )

;;; Convert a name and a class into a field.
(define (retrieve-named-field class name)
  (let search ((fields (careless-Class-fields class)))
    (and (pair? fields)
         (if (eq? name (careless-Field-name (car fields)))
             (car fields)
             (search (cdr fields)) ) ) ) )

;;; Create the definition of a field reader. Try to create a direct
;;; reader otherwise use the general field-value function.
(define (make-reader field)
  (let ((class (Field-defining-class field)))
    (let skip ((fields (careless-Class-fields class))
               (offset *starting-offset*) )
      (if (eq? field (car fields))
          (cond ((Mono-Field? (car fields))
                 (lambda (o)
                   (check-class-membership o class)
                   (vector-ref o offset) ) )
                ((Poly-Field? (car fields))
                 (lambda (o i)
                   (check-class-membership o class)
                   (check-index-range i o offset)
                   (vector-ref o (+ offset 1 i)) ) ) )
          (cond ((Mono-Field? (car fields)) 
                 (skip (cdr fields) (+ 1 offset)) )
                ((Poly-Field? (car fields)) 
                 (cond ((Mono-Field? field)
                        (lambda (o)
                          (field-value o field) ) )
                       ((Poly-Field? field)
                        (lambda (o i)
                          (field-value o field i) ) ) ) ) ) ) ) ) )

;;; Compute a real offset in an instance to the beginning of a Field.
;;; If it is a poly-field then the offset refers to the length of the
;;; poly-field.
(define (compute-field-offset o field)
  (let ((class (Field-defining-class field)))
    ;; (assume (check-class-membership o class))
    (let skip ((fields (careless-Class-fields class))
               (offset *starting-offset*) )
      (if (eq? field (car fields))
          offset
          (cond 
           ((Mono-Field? (car fields))
            (skip (cdr fields) (+ 1 offset)) )
           ((Poly-Field? (car fields))
            (skip (cdr fields) 
                  (+ 1 offset (vector-ref o offset)) ) ) ) ) ) ) )

;;; A general reader. An extra integer is given to access a
;;; Poly-Field, otherwise it is silently ignored.
(define (field-value o field . i)
  (let ((class (Field-defining-class field)))
    (check-class-membership o class)
    (let ((fields (careless-Class-fields class))
          (offset (compute-field-offset o field)) )
      (cond ((Mono-Field? field)
             (vector-ref o offset) )
            ((Poly-Field? field)
             (check-index-range (car i) o offset)
             (vector-ref o (+ offset 1 (car i))) ) ) ) ) ) 

;;; Create the definition of a field writer.
(define (make-writer field)
  (let ((class (Field-defining-class field)))
    (let skip ((fields (careless-Class-fields class))
               (offset *starting-offset*) )
      (if (eq? field (car fields))
          (cond ((Mono-Field? (car fields))
                 (lambda (o v)
                   (check-class-membership o class)
                   (vector-set! o offset v) ) )
                ((Poly-Field? (car fields))
                 (lambda (o i v)
                   (check-class-membership o class)
                   (check-index-range i o offset)
                   (vector-set! o (+ offset 1 i) v) ) ) )
          (cond ((Mono-Field? (car fields)) 
                 (skip (cdr fields) (+ 1 offset)) )
                ((Poly-Field? (car fields)) 
                 (cond ((Mono-Field? field)
                        (lambda (o v)
                          (set-field-value! o v field) ) )
                       ((Poly-Field? field)
                        (lambda (o i v)
                          (set-field-value! o v field i) ) ) )
                 ) ) ) ) ) )

;;; A general writer. An extra integer is given to access a
;;; Poly-Field, otherwise it is silently ignored.
(define (set-field-value! o v field . i)
  (let ((class (Field-defining-class field)))
    (check-class-membership o class)
    (let ((fields (careless-Class-fields class))
          (offset (compute-field-offset o field)) )
      (cond ((Mono-Field? field)
             (vector-set! o offset v) )
            ((Poly-Field? field)
             (check-index-range (car i) o offset)
             (vector-set! o (+ offset 1 (car i)) v) ) ) ) ) )

;;; Check wether an index is valid.
(define (check-index-range i o offset)
  (let ((size (vector-ref o offset)))
    (if (not (and (>= i 0) (< i size)))
      (meroonet-error "Out of range index" i size) ) ) )

;;; Create the definition of a Poly-Field lengther. Try to create a direct
;;; reader otherwise use the general field-value function.
(define (make-lengther field)
  ;; (assume (Poly-Field? field))
  (let ((class (Field-defining-class field)))
    (let skip ((fields (careless-Class-fields class))
               (offset *starting-offset*) )
      (if (eq? field (car fields))
          (lambda (o)
            (check-class-membership o class)
            (vector-ref o offset) )
          (cond ((Mono-Field? (car fields)) 
                 (skip (cdr fields) (+ 1 offset)) )
                ((Poly-Field? (car fields)) 
                 (lambda (o) (field-length o field)) ) ) ) ) ) )

;;; The generic accessor to know the length of a poly-field
(define (field-length o field)
  (let* ((class (Field-defining-class field))
         (fields (careless-Class-fields class))
         (offset (compute-field-offset o field)) )
    (check-class-membership o class)
    (vector-ref o offset) ) )

;;;==================================================== Class creation
;;; Create a class instance and graft it into the inheritance hierarchy.
(define (register-class name supername own-field-descriptions)
  (Class-initialize! (allocate-Class) 
                     name
                     (->Class supername)
                     own-field-descriptions ) )

;;; Fill all the fields of the bare class and insert it into the
;;; inheritance tree.
(define (Class-initialize! class name superclass 
                           own-field-descriptions )
  (set-Class-number! class *class-number*)
  (set-Class-name! class name)
  (set-Class-superclass! class superclass)
  (set-Class-subclass-numbers! class '())
  (set-Class-fields! 
   class (append (Class-fields superclass)
                 (parse-fields class own-field-descriptions) ) )
  ;; install definitely the class
  (set-Class-subclass-numbers!
   superclass 
   (cons *class-number* (Class-subclass-numbers superclass)) )
  (vector-set! *classes* *class-number* class)
  (set! *class-number* (+ 1 *class-number*))
  ;; propagate the methods of the super to the fresh class
  (update-generics class)
  class )

;;; Parse the description of the fields, check them and install them
;;; in the class instance.
(define (parse-fields class own-field-descriptions)
  (define (Field-initialize! field name)
    (check-conflicting-name class name)
    (set-Field-name! field name)
    (set-Field-defining-class-number! field (Class-number class))
    field )
  (define (parse-Mono-Field name)
    (Field-initialize! (allocate-Mono-Field) name) )
  (define (parse-Poly-Field name)
    (Field-initialize! (allocate-Poly-Field) name) )
  (if (pair? own-field-descriptions)
      (cons (cond 
             ((symbol? (car own-field-descriptions))
              (parse-Mono-Field (car own-field-descriptions)) )
             ((pair? (car own-field-descriptions))
              (case (caar own-field-descriptions)
                ((=) (parse-Mono-Field 
                      (cadr (car own-field-descriptions)) ))
                ((*) (parse-Poly-Field
                      (cadr (car own-field-descriptions)) ))
                (else (meroonet-error 
                       "Erroneous field specification"
                       (car own-field-descriptions) )) ) ) )
            (parse-fields class (cdr own-field-descriptions)) )
      '() ) )

;;; Tests that no preceding field has the same name.
(define (check-conflicting-name class fname)
  (let check ((fields (careless-Class-fields 
                       (Class-superclass class) )))
    (if (pair? fields)
        (if (eq? (careless-Field-name (car fields)) fname)
            (meroonet-error "Duplicated field name" fname)
            (check (cdr fields)) )
        #t ) ) )

;;; Extract from a field the class that defines it.
(define (Field-defining-class field)
  (number->class (careless-Field-defining-class-number field)) )

;;;==================================================== Initial functions
;;; Define by hand the functions that are related to the hand-built classes.

(define Class? (make-predicate Class-class))
(define Generic? (make-predicate Generic-class))
(define Field? (make-predicate Field-class))
(define Mono-Field? (make-predicate Mono-Field-class))
(define Poly-Field? (make-predicate Poly-Field-class))

(define Class-name
  (make-reader (retrieve-named-field Class-class 'name)))
(define set-Class-name!
  (make-writer (retrieve-named-field Class-class 'name)))
(define Class-number
  (make-reader (retrieve-named-field Class-class 'number)))
(define set-Class-number!
  (make-writer (retrieve-named-field Class-class 'number)))
(define Class-fields
  (make-reader (retrieve-named-field Class-class 'fields)))
(define set-Class-fields!
  (make-writer (retrieve-named-field Class-class 'fields)))
(define Class-superclass
  (make-reader (retrieve-named-field Class-class 'superclass)))
(define set-Class-superclass!
  (make-writer (retrieve-named-field Class-class 'superclass)))
(define Class-subclass-numbers
  (make-reader (retrieve-named-field Class-class 'subclass-numbers)))
(define set-Class-subclass-numbers!
  (make-writer (retrieve-named-field Class-class 'subclass-numbers)))
;;; Since Class-fields now exists, these can be defined.  Allocation
;;; functions make-maker and make-allocator can be programmed without
;;; careless-Class-fields and relative functions.
(define make-Class (make-maker Class-class))
(define allocate-Class (make-allocator Class-class))

(define make-Generic (make-maker Generic-class))
(define allocate-Generic (make-allocator Generic-class))
(define Generic-name
  (make-reader (retrieve-named-field Generic-class 'name)))
(define set-Generic-name!
  (make-writer (retrieve-named-field Generic-class 'name)))
(define Generic-default
  (make-reader (retrieve-named-field Generic-class 'default)))
(define set-Generic-default!
  (make-writer (retrieve-named-field Generic-class 'default)))
(define Generic-dispatch-table
  (make-reader (retrieve-named-field Generic-class 'dispatch-table)))
(define set-Generic-dispatch-table!
  (make-writer (retrieve-named-field Generic-class 'dispatch-table)))
(define Generic-signature
  (make-reader (retrieve-named-field Generic-class 'signature)))
(define set-Generic-signature!
  (make-writer (retrieve-named-field Generic-class 'signature)))
 
;;; Cannot instantiate Field
;(define make-Field (make-maker Field-class))
;(define allocate-Field (make-allocator Field-class))
(define Field-name
  (make-reader (retrieve-named-field Field-class 'name)))
(define set-Field-name!
  (make-writer (retrieve-named-field Field-class 'name)))
(define Field-defining-class-number
  (make-reader (retrieve-named-field Field-class 'defining-class-number)))
(define set-Field-defining-class-number!
  (make-writer (retrieve-named-field Field-class 'defining-class-number)))

(define make-Mono-Field (make-maker Mono-Field-class))
(define allocate-Mono-Field (make-allocator Mono-Field-class))
;;; do not define Mono-Field-name and other accessors since they are
;;; nearly equivalent to Field-name and others: this saves some room.

(define make-Poly-Field (make-maker Poly-Field-class))
(define allocate-Poly-Field (make-allocator Poly-Field-class))

;;;==================================================== Generic functions
;;; A generic function is represented by a closure that encloses an
;;; instance of Generic which itself contains a dispatch table. This
;;; table is a vector containing methods that are retrieved using the
;;; number associated to class. 

;;; Syntax: (define-generic ( <name-of-the-generic-function>
;;;                           . <description-of-the-variables> )
;;;            [ <optional-default-method> ]   )
;;; <description-of-the-variables> 
;;;     ::= ( <variable-name>     . <description-of-the-variables> )
;;;      |  ( ( <variable-name> ) . <description-of-the-variables> )
;;;      |  <variable-name>
;;; The discriminating variable appears within parentheses but without class.

;;; Define a generic function. 
(define-meroonet-macro (define-generic call . body)
  (parse-variable-specifications
   (cdr call)
   (lambda (discriminant variables)
     (let ((generic (gensym)))          ; make generic hygienic
       `(define ,(car call)
          (let ((,generic 
                 (register-generic 
                  ',(car call) 
                  (lambda ,(flat-variables variables)
                    ,(if (pair? body)
                         `(begin . ,body)
                         `(meroonet-error 
                           "No method" ',(car call)
                           . ,(flat-variables variables) ) ) ) 
                  ',(cdr call) )))
            (lambda ,variables
              ((determine-method ,generic ,(car discriminant))
               . ,(flat-variables variables) ) ) ) ) ) ) ) )

;;; Create and register a generic function.
(define (register-generic generic-name default signature)
  (let* ((dispatch-table (make-vector *maximal-number-of-classes*
                                      default )) 
         (generic (make-Generic generic-name
                                default
                                dispatch-table
                                signature )) )
    (set! *generics* (cons generic *generics*))
    generic ) )

;;; Parse a list of specifications, extract all the variables and the
;;; discriminating variable among them. 
;;; Restriction: Silently ignore if there is more than one
;;; discriminant variable ie multi-methods are not implemented.
(define (parse-variable-specifications specifications k)
  (if (pair? specifications)
      (parse-variable-specifications
       (cdr specifications)
       (lambda (discriminant variables)
         (if (pair? (car specifications))
             (k (car specifications) 
                (cons (caar specifications) variables) )
             (k discriminant (cons (car specifications) 
                                   variables )) ) ) )
      (k #f specifications) ) )

;;; Flatten a list of variables to avoid a final dotted variable. This
;;; is necessary since all methods (even the default method) are
;;; represented by closures with a fix arity.
(define (flat-variables variables)
  (if (pair? variables)
      (cons (car variables) (flat-variables (cdr variables)))
      (if (null? variables) variables (list variables)) ) )

;;; Propagate methods for the fresh class from its superclass.
(define (update-generics class)
  (let ((superclass (Class-superclass class)))
    (for-each (lambda (generic)
                (vector-set! (Generic-dispatch-table generic)
                             (Class-number class)
                             (vector-ref 
                              (Generic-dispatch-table generic)
                              (Class-number superclass) ) ) )
              *generics* ) ) )
  
;;;==================================================== Method creation
;;; Syntax: (define-method ( <name-of-the-generic-function
;;;                          . <description-of-the-variables> )
;;;              <body-of-the-method> )
;;; <description-of-the-variables> 
;;;     ::= ( <variable-name> . <description-of-the-variables> )
;;;      |  ( ( <variable-name> <class-name> ) 
;;;           . <description-of-the-variables> )
;;;      |  <variable-name>
;;; The discriminating variable appears with a class.

;;; Define a method on a generic function. The dispatch table to
;;; update is found via the name of the generic function in the list
;;; of all generic instances. 
(define-meroonet-macro (define-method call . body)
  (parse-variable-specifications
   (cdr call)
   (lambda (discriminant variables)
     (let ((g (gensym))(c (gensym)))    ; make g and c hygienic
       `(register-method
         ',(car call)
         (lambda (,g ,c)
           (lambda ,(flat-variables variables)
             (define (call-next-method)
               ((if (Class-superclass ,c)
                    (vector-ref (Generic-dispatch-table ,g) 
                                (Class-number 
                                 (Class-superclass ,c) ) )
                    (Generic-default ,g) )
                . ,(flat-variables variables) ) )
             . ,body ) )
         ',(cadr discriminant)
         ',(cdr call) ) ) ) ) )

;;; Add a new method for a class into a generic function. Pay
;;; attention to propagate it to all its subclasses that do not
;;; redefine it. This is a case where functions are compared with eq?.
(define (register-method generic-name pre-method class-name signature)
  (let* ((generic (->Generic generic-name))
         (class (->Class class-name))
         (new-method (pre-method generic class))
         (dispatch-table (Generic-dispatch-table generic))
         (old-method (vector-ref dispatch-table 
                                 (Class-number class) )) )
    (check-signature-compatibility generic signature)
    (let propagate ((cn (Class-number class)))
      (let ((content (vector-ref dispatch-table cn)))
        (if (eq? content old-method)
            (begin
              (vector-set! dispatch-table cn new-method)
              (for-each 
               propagate
               (Class-subclass-numbers 
                (number->class cn) ) ) ) ) ) ) ) )

;;; Check signature compatibility between a generic function and a method.
;;; The two signatures can have both a dotted variable.
(define (check-signature-compatibility generic signature)
  (define (coherent-signatures? la lb)
    (if (pair? la)
        (if (pair? lb)
            (and (or ;; similar discriminating variable
                     (and (pair? (car la)) (pair? (car lb)))
                     ;; similar regular variable
                     (and (symbol? (car la)) (symbol? (car lb))) )
                 (coherent-signatures? (cdr la) (cdr lb)) )
            #f )
        (or (and (null? la) (null? lb))
            ;; similar dotted variable
            (and (symbol? la) (symbol? lb)) ) ) )
  (if (not (coherent-signatures? (Generic-signature generic) 
                                 signature ))
    (meroonet-error "Incompatible signatures" generic signature) ) )

;;; Determine the method to apply in a generic function. Choose the
;;; default if the discriminating variable does not contain a Meroonet
;;; object otherwise find the method in the dispatch-table with the
;;; number associated to its class. The generic instance is
;;; dereferenced to obtain the dispatch-table or the default so these
;;; can be changed from outside.
(define (determine-method generic o)
  (if (Object? o)
      (vector-ref (Generic-dispatch-table generic)
                  (vector-ref o 0) )
      (Generic-default generic) ) )

;;; end of meroonet.scm
