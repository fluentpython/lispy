;;; same as DEA/meroon/oo-tests.scm but removed some tests and adapted others.

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;;        Testing Meroonet

;;; Testing symbol->class
(->Class 'Class)
---
; Not an error in Meroonet
;(->Class 'FoO)
;***
; Only monadic in Meroonet
;(->Class 'FoO (lambda (name) 3))
;3

;;; testing some initial relationship
(Class-name (->Class 'Object))
Object
(Class-name (->Class 'Class))
Class
(eq? (Class-superclass (->Class 'Class)) (->Class 'Object))
#t

;;; testing object?
(Object? (->Class 'Object))
   #t
(Object? (->Class 'Class))
   #t
(Object? #t)
   #f
(Object? '())
   #f
(Object? 3)
   #f
(Object? (vector 'a 'b 'c))
   #f
   
;;; testing Class?
(Class? (->Class 'Class))
   #t
(Class? (->Class 'Object))
   #t
(Class? #t)
   #f
(Class? 33)
   #f

;;; testing define-class and generated functions
(define-class Point Object (x y))
   ---
;;;;(set! the-Point (make-Point 2 44))
;;;;   ---
;;;;(Point-x the-Point)
;;;;   2
;;;;(Point-y the-Point)
;;;;   44
;;;;(Point? the-Point)
;;;;   #t
;;;;(Object? the-Point)
;;;;   #t
;;;;(Class? the-Point)
;;;;   #f
;;;;(allocate-Point)
;;;;   ---
;;;;(begin (set-Point-x! the-Point 3) (Point-x the-Point))
;;;;   3
;;;;(begin (set-Point-y! the-Point 55) (Point-y the-Point))
;;;;   55

;;; testing a subclass of the previous one and related functions
(define-class ColoredPoint Point (color))
   ---
;(Class-fields (->Class 'ColoredPoint))
;   (x y color)
;;;;(set! the-Point (make-ColoredPoint 2 44 'white))
;;;;   ---
;;;;(Point-x the-Point)
;;;;   2
;;;;(Point-y the-Point)
;;;;   44
;;;;(Point? the-Point)
;;;;   #t
;;;;(ColoredPoint? the-Point)
;;;;   #t
;;;;(Object? the-Point)
;;;;   #t
;;;;(Class? the-Point)
;;;;   #f
;;;;(allocate-ColoredPoint)
;;;;   ---
;;;;(begin (set-Point-x! the-Point 3) (Point-x the-Point))
;;;;   3
;;;;(begin (set-Point-y! the-Point 55) (Point-y the-Point))
;;;;   55
;;;;(begin (set-Point-x! the-Point 33) (ColoredPoint-x the-Point))
;;;;   33
;;;;(begin (set-Point-y! the-Point 5) (ColoredPoint-y the-Point))
;;;;   5
;;;;(begin (set-ColoredPoint-x! the-Point 3)(ColoredPoint-x the-Point))
;;;;   3
;;;;(begin (set-ColoredPoint-y! the-Point 55) (ColoredPoint-y the-Point))
;;;;   55
;;;;(begin (set-ColoredPoint-x! the-Point 33) (Point-x the-Point))
;;;;   33
;;;;(begin (set-ColoredPoint-y! the-Point 5) (Point-y the-Point))
;;;;   5
;;;;(ColoredPoint-color the-Point)
;;;;   white
;;;;(begin (set-ColoredPoint-color! the-Point 'black) 
;;;;       (ColoredPoint-color the-Point) )
;;;;   black

;;; Testing vectorship 
(define-class NamedColoredPoint ColoredPoint ((* names)))
   ---
;;;;(set! the-Point 
;;;;      (make-NamedColoredPoint 
;;;;       2 44 'pink 
;;;;            3 'joe 'jill 'jack ) )
;;;;   ---
;;;;(NamedColoredPoint-names-length the-Point)
;;;;   3
;;;;(NamedColoredPoint-names the-Point 1)
;;;;   jill
;;;;(set-NamedColoredPoint-names! the-Point 0 'jean)
;;;;   ---
;;;;(NamedColoredPoint-names the-Point 0)
;;;;   jean
;;;;(NamedColoredPoint-names the-Point 2)
;;;;   jack
;;;;(Point-x the-Point)
;;;;   2
;;;;(Point-y the-Point)
;;;;   44
;;;;(Point? the-Point)
;;;;   #t
;;;;(ColoredPoint? the-Point)
;;;;   #t
;;;;(NamedColoredPoint? the-Point)
;;;;   #t
;;;;(Object? the-Point)
;;;;   #t
;;;;(Class? the-Point)
;;;;   #f
;;;;(ColoredPoint-color the-Point)
;;;;   pink
;;;;(allocate-NamedColoredPoint 3)
;;;;   ---
;;;;(begin (set-Point-x! the-Point 3) (Point-x the-Point))
;;;;   3
;;;;(begin (set-Point-y! the-Point 55) (Point-y the-Point))
;;;;   55
;;;;(begin (set-Point-x! the-Point 33) (ColoredPoint-x the-Point))
;;;;   33
;;;;(begin (set-Point-y! the-Point 5) (ColoredPoint-y the-Point))
;;;;   5
;;;;(begin (set-ColoredPoint-x! the-Point 3)(ColoredPoint-x the-Point))
;;;;   3
;;;;(begin (set-ColoredPoint-y! the-Point 55) (ColoredPoint-y the-Point))
;;;;   55
;;;;(begin (set-ColoredPoint-x! the-Point 33) (Point-x the-Point))
;;;;   33
;;;;(begin (set-ColoredPoint-y! the-Point 5) (Point-y the-Point))
;;;;   5
;;;;(ColoredPoint-color the-Point)
;;;;   pink
;;;;(begin (set-ColoredPoint-color! the-Point 'black) 
;;;;       (ColoredPoint-color the-Point) )
;;;;   black
;;;;(set! the-Point (make-NamedColoredPoint 1 33 'pink 0))
;;;;   ---
;;;;(NamedColoredPoint-names-length the-Point)
;;;;   0
;;;;(NamedColoredPoint-names the-Point 0)
;;;;   ***
;;;;(NamedColoredPoint-names the-Point 3)
;;;;   ***
;;;;(set-NamedColoredPoint-names! the-Point 0 'jill)
;;;;   ***
;;;;(make-NamedColoredPoint 3 2 1 'foo)
;;;;   *** ; not a repetition factor
;; NOTE: This error is caught by list-tail which is invoked with a non numeric
;; second argument. Your test-meroonet function should catch these errors
;; by gaining control on the errors detected by the underlying Scheme.
;;;;(make-NamedColoredPoint 3 2 1 2 'foo)
;;;;   *** ; too less
; Not tested in Meroonet
;(make-NamedColoredPoint 3 2 1 2 'foo 'bar 'hux)
;   *** ; too much
;;;;(set! the-Point (make-NamedColoredPoint 1 33 'pink 1 'joe))
;;;;   ---
;;;;(NamedColoredPoint-names-length the-Point)
;;;;   1
;;;;(NamedColoredPoint-names the-Point 0)
;;;;   joe
;;;;(NamedColoredPoint-names the-Point 1)
;;;;   ***
;;;;(set-NamedColoredPoint-names! the-Point 0 'jill)
;;;;   ---
;;;;(NamedColoredPoint-names the-Point 0)
;;;;   jill
;;;;(set-NamedColoredPoint-names! the-Point 1 'jack)
;;;;   ***
;;;;(NamedColoredPoint-names the-Point 0)
;;;;   jill
;;;;(set! the-Point (allocate-NamedColoredPoint 2))
;;;;   ---
;;;;(set-NamedColoredPoint-names! the-Point 0 'jill)
;;;;   ---
;;;;(NamedColoredPoint-names the-Point 0)
;;;;   jill
;;;;(set-NamedColoredPoint-names! the-Point 1 'jack)
;;;;   ---
;;;;(NamedColoredPoint-names the-Point 1)
;;;;   jack
;;;;(set-NamedColoredPoint-names! the-Point 2 'joe)
;;;;   ***
(NamedColoredPoint-names the-Point 2)
   ***



;;; Testing vectorship inheritance 
;;; Before this expression, syntax-case works correctly on (when 1 2 3)
(define-class NickNamedColoredPoint NamedColoredPoint ((* nicknames)))
   ---
;;; and after it acts erroneously.
(set! the-Point 
      (make-NickNamedColoredPoint 
       2 44 'pink 
            3 'joe 'jill 'jack
            2 'adam 'eve ) )
   ---
(NamedColoredPoint-names-length the-Point)
   3
(NickNamedColoredPoint-names-length the-Point)
   3
(NickNamedColoredPoint-nicknames-length the-Point)
   2
(NamedColoredPoint-names the-Point 1)
   jill
(NickNamedColoredPoint-names the-Point 1)
   jill
(NickNamedColoredPoint-names the-Point 2)
   jack
(NickNamedColoredPoint-nicknames the-Point 0)
   adam
(NickNamedColoredPoint-nicknames the-Point 1)
   eve
;;;;(NickNamedColoredPoint-nicknames the-Point 2)
;;;;   ***
(set-NamedColoredPoint-names! the-Point 0 'jill)
   ---
(NamedColoredPoint-names the-Point 0)
   jill
(set-NickNamedColoredPoint-names! the-Point 0 'jill)
   ---
(NickNamedColoredPoint-names the-Point 0)
   jill
(set-NickNamedColoredPoint-nicknames! the-Point 0 'chaim)
   ---
(NickNamedColoredPoint-nicknames the-Point 0)
   chaim
(Point-x the-Point)
   2
(Point-y the-Point)
   44
(Point? the-Point)
   #t
(ColoredPoint? the-Point)
   #t
(NamedColoredPoint? the-Point)
   #t
(Object? the-Point)
   #t
(Class? the-Point)
   #f
(ColoredPoint-color the-Point)
   pink
(allocate-NickNamedColoredPoint 3 2)
   ---
(begin (set-Point-x! the-Point 3) (Point-x the-Point))
   3
(begin (set-Point-y! the-Point 55) (Point-y the-Point))
   55
(begin (set-Point-x! the-Point 33) (ColoredPoint-x the-Point))
   33
(begin (set-Point-y! the-Point 5) (ColoredPoint-y the-Point))
   5
(begin (set-ColoredPoint-x! the-Point 3)(ColoredPoint-x the-Point))
   3
(begin (set-ColoredPoint-y! the-Point 55) (ColoredPoint-y the-Point))
   55
(begin (set-ColoredPoint-x! the-Point 33) (Point-x the-Point))
   33
(begin (set-ColoredPoint-y! the-Point 5) (Point-y the-Point))
   5
(begin (set-ColoredPoint-color! the-Point 'black) 
       (ColoredPoint-color the-Point) )
   black
(set! the-Point (make-NickNamedColoredPoint 1 33 'pink 0 0))
   ---
(NickNamedColoredPoint-names-length the-Point)
   0
(NickNamedColoredPoint-nicknames-length the-Point)
   0
;;;;(NickNamedColoredPoint-names the-Point 1)
;;;;   ***
;;;;(NickNamedColoredPoint-names the-Point 3)
;;;;   ***
;;;;(set-NickNamedColoredPoint-names! the-Point 0 'jill)
;;;;   ***
;;;;(set-NickNamedColoredPoint-names! the-Point 1 'jill)
;;;;   ***
;;;;(NickNamedColoredPoint-nicknames the-Point 3)
;;;;   ***
;;;;(set-NickNamedColoredPoint-nicknames! the-Point 1 'jill)
;;;;   ***
;;;;(make-NickNamedColoredPoint 3 2 1 'foo)
;;;;   *** ; not a repetition factor
;;;;(make-NickNamedColoredPoint 3 2 1 2 'foo)
;;;;   *** ; too less
;;;;(make-NickNamedColoredPoint 3 2 1 2 'foo 'bar 'hux)
;;;;   *** ; not a repetition factor
; not checked in Meroonet
;(make-NickNamedColoredPoint 3 2 1 2 'foo 'bar 0 'hux)
;   *** ; too much
;;;;(make-NickNamedColoredPoint 3 2 1 2 'foo 'bar 2 'hux)
;;;;   *** ; too less
; not checked in Meroonet
;(make-NickNamedColoredPoint 3 2 1 2 'foo 'bar 2 'hux 'bar 'wrek 'quux)
;   *** ; too much

;;; test fields after two sequences
(define-class extra-NickNamedColoredPoint NickNamedColoredPoint
  (extra1 extra2) )
   ---
(set! the-Point 
      (make-extra-NickNamedColoredPoint 
       2 44 'pink 
            3 'joe 'jill 'jack
            2 'adam 'eve 
            'apiali 'docious ) )
   ---
(extra-NickNamedColoredPoint-extra2 the-Point)
   docious
(set-extra-NickNamedColoredPoint-extra2! the-Point 'foo)
   ---
(extra-NickNamedColoredPoint-extra2 the-Point)
   foo
(NamedColoredPoint-names-length the-Point)
   3
(extra-NickNamedColoredPoint-names-length the-Point)
   3
(extra-NickNamedColoredPoint-nicknames-length the-Point)
   2
(NamedColoredPoint-names the-Point 1)
   jill
(extra-NickNamedColoredPoint-names the-Point 1)
   jill
(extra-NickNamedColoredPoint-nicknames the-Point 1)
   eve
(set-NamedColoredPoint-names! the-Point 0 'jill)
   ---
(NamedColoredPoint-names the-Point 0)
   jill
(set-extra-NickNamedColoredPoint-names! the-Point 0 'jill)
   ---
(extra-NickNamedColoredPoint-names the-Point 0)
   jill
(set-extra-NickNamedColoredPoint-nicknames! the-Point 0 'chaim)
   ---
(extra-NickNamedColoredPoint-nicknames the-Point 0)
   chaim
(Point-x the-Point)
   2
(Point-y the-Point)
   44
(Point? the-Point)
   #t
(ColoredPoint? the-Point)
   #t
(NamedColoredPoint? the-Point)
   #t
(Object? the-Point)
   #t
(Class? the-Point)
   #f
(ColoredPoint-color the-Point)
   pink
(allocate-extra-NickNamedColoredPoint 3 2)
   ---
(begin (set-Point-x! the-Point 3) (Point-x the-Point))
   3
(begin (set-Point-y! the-Point 55) (Point-y the-Point))
   55
(begin (set-Point-x! the-Point 33) (ColoredPoint-x the-Point))
   33
(begin (set-Point-y! the-Point 5) (ColoredPoint-y the-Point))
   5
(begin (set-ColoredPoint-x! the-Point 3)(ColoredPoint-x the-Point))
   3
(begin (set-ColoredPoint-y! the-Point 55) (ColoredPoint-y the-Point))
   55
(begin (set-ColoredPoint-x! the-Point 33) (Point-x the-Point))
   33
(begin (set-ColoredPoint-y! the-Point 5) (Point-y the-Point))
   5
(begin (set-ColoredPoint-color! the-Point 'black) 
       (ColoredPoint-color the-Point) )
   black
(set! the-Point (make-NickNamedColoredPoint 1 33 'pink 0 0))
   ---
(extra-NickNamedColoredPoint-names-length the-Point)
   0
(extra-NickNamedColoredPoint-nicknames-length the-Point)
   0
;;;;(extra-NickNamedColoredPoint-names the-Point 3)
;;;;   ***
;;;;(set-extra-NickNamedColoredPoint-names! the-Point 0 'jill)
;;;;   ***
;;;;(extra-NickNamedColoredPoint-nicknames the-Point 3)
;;;;   ***
;;;;(set-extra-NickNamedColoredPoint-nicknames! the-Point 0 'jill)
;;;;   ***
;;;;(make-extra-NickNamedColoredPoint 3 2 1 'foo)
;;;;   *** ; not a repetition factor
;;;;(make-extra-NickNamedColoredPoint 3 2 1 2 'foo)
;;;;   *** ; too less
;;;;(make-extra-NickNamedColoredPoint 3 2 1 2 'foo 'bar 'hux)
;;;;   *** ; not a repetition factor
; not checked in Meroonet
;(make-extra-NickNamedColoredPoint 3 2 1 2 'foo 'bar 0 'hux)
;   *** ; too much
;;;;(make-extra-NickNamedColoredPoint 3 2 1 2 'foo 'bar 2 'hux)
;;;;   *** ; too less
(make-extra-NickNamedColoredPoint 3 2 1 2 'foo 'bar 2 'hux 'bar 'wrek 'quux)
   ---
; not checked in Meroonet
;(make-extra-NickNamedColoredPoint 3 2 1 2 'foo 'bar 
; 2 'hux 'bar 'wrek 'quux 55)
;   *** ; too much
  

;;; generic functions
; not defined in Meroonet
;(initialize! 3)
;   3
;(initialize! #t)
;   #t
;(initialize! the-Point)
;   ---
;(show (->Class 'Class))
;   ---
;(show the-Point)
;   ---
;(show 34)
;   ---
;(show 3.14)
;   ---
;(show #\space)
;   ---
;(show #\a)
;   ---
;(show (list #t #f '()))
;   ---
;(equal? (clone (->Class 'Class)) (->Class 'Class))
;   #t
;(let ((pt (make-Point 1 2)))
;  (equal? pt (clone pt)) )
;   #t
;(clone 3) ; 3 is not an instance
;   ***
   
;;; generic with default method, discriminating on the second variable
(define-generic (foo x (y) z)
  (list x y z) )
   ---
(foo 3 the-Point 4)
   ---
(length (foo 3 the-Point 4))
   3
(foo 3 4 5) ; 4 is not an instance but there is a default method
   (3 4 5) 
   
;;; Adding methods
(define-method (foo x (o ColoredPoint) z)
  (ColoredPoint-x o) )
   ---
(let ((pt (make-Point 1 2)))
  (length (foo 9 pt 8)) )
   3
(let ((pt (make-ColoredPoint 1 2 'red)))
  (foo 9 pt 8) )
   1
(let ((pt (make-NamedColoredPoint 1 2 'red 1 'jill)))
  (foo 9 pt 8) )
   1

;;; Adding a method above
(define-method (foo x (o Object) z)
  'foo )
   ---
(let ((pt (make-Point 1 2)))
  (foo 9 pt 8) )
   foo
(let ((pt (make-ColoredPoint 1 2 'red)))
  (foo 9 pt 8) )
   1
(let ((pt (make-NamedColoredPoint 1 2 'red 1 'jill)))
  (foo 9 pt 8) )
   1

;;; redefining method
(define-method (foo x (o ColoredPoint) z)
  (ColoredPoint-y o) )
   ---
(let ((pt (make-Point 1 2)))
  (foo 9 pt 8) )
   foo
(let ((pt (make-ColoredPoint 1 2 'red)))
  (foo 9 pt 8) )
   2
(let ((pt (make-NamedColoredPoint 1 2 'red 1 'jill)))
  (foo 9 pt 8) )
   2

;;; Incoherent method
;;;;(define-method (foo x) 3)
;;;;   ***
;;;;(define-method (foo (x) y z) 4)
;;;;   ***
;;;;(define-method (foo x (y) z . others) 5)
;;;;   ***
   
(define-class Point-3d Point (z))
   ---

;;; Redefinition of a class (exists no more in Meroonet)
;(define-class Point-3d Point (h))
;   ---

;;; Show guts (exists no more in Meroonet)
;(show *classes*)
;   ---
;(show (Class-fields (->Class 'NamedColoredPoint)))
;   ---
;(show-hierarchy)
;   ---
;(show-hierarchy 'Object)   ; same as above
;   ---
;(show-hierarchy 'Field)
;   ---
;(show-generic 'show)
;   ---

;;; Tracing generic functions (exists no more in Meroonet)
(define-generic (foo (x) y z) `default)
   ---
(foo 1 2 3)
   default
(define-method (foo (x Point) y z) `point)
   ---
(foo (make-Point 1 2) 3 4)
   point
;(generic-trace (->Generic 'foo)
;               (lambda (x y z) (set! foo-args `call))
;               (lambda (result) 
;                 `(return ,result) ) )
;   ---
;(set! foo-args 'void)
;   ---
;(foo 'a 'b 'c)
;   (return default)
;foo-args
;   call
;(set! foo-args 'void)
;   ---
;(foo (make-Point 1 2) 3 4)
;   (return point)
;foo-args
;   call
;(generic-untrace (->Generic 'foo))
;   ---
;(set! foo-args 'void)
;   ---
;(foo 'a 'b 'c)
;   default
;foo-args
;   void
;(foo (make-Point 1 2) 3 4)
;   point

;;; Tracing generic functions with a dotted variable list
(define-generic (foo (x) y . z) `(default . ,z))
   ---
(foo 1 2)
   (default)
(foo 1 2 3)
   (default 3)
(foo 1 2 3 4 5)
   (default 3 4 5)
(define-method (foo (x Point) y . z) `(point . ,z))
   ---
(foo (make-Point 1 2) 3)
   (point)
(foo (make-Point 1 2) 3 4)
   (point 4)
(foo (make-Point 1 2) 3 4 5 6)
   (point 4 5 6)
;(generic-trace (->Generic 'foo)
;               (lambda (x y . z) (set! foo-args `call))
;               (lambda (result) 
;                 `(return ,result) ) )
;   ---
;(set! foo-args 'void)
;   ---
;(foo 1 2)
;   (return (default))
;foo-args
;   call
;(set! foo-args 'void)
;   ---
;(foo (make-Point 1 2) 3 4 5)
;   (return (point 4 5))
;foo-args
;   call
;(generic-untrace (->Generic 'foo))
;   ---
;(set! foo-args 'void)
;   ---
;(foo 'a 'b 'c)
;   (default c)
;foo-args
;   void
;(foo (make-Point 1 2) 3 4)
;   (point 4)


;;; testing call-next-method
(define-generic (bar (o)) 'root)
   ---
(define-method (bar (o Object)) 'object)
   ---
(define-method (bar (o ColoredPoint))
  (cons (call-next-method) (call-next-method)) )
   ---
(bar (allocate-ColoredPoint))
   (object . object)
(define-method (bar (o Point)) 'point)
   ---
(bar (allocate-ColoredPoint))
   (point . point)


;;; Testing meta object protocol (Not in Meroonet)
;((Class-predicate (->Class 'Point)) (make-Point 1 2))
;   #t
;((Class-predicate (->Class 'ColoredPoint)) (make-Point 1 2))
;   #f
;(Point? ((Class-maker (->Class 'Point)) 1 2))
;   #t
;(Point? ((Class-allocator (->Class 'Point))))
;   #t

;;; Testing some errors
;(make-Class 'foo) ;; cannot handle this error
;   ***  ; too less arguments
;;;;(define-class foo Point (x))
;;;;   *** ; redefinition of X

;;; Testing mutability of fields     (Not in Meroonet)
;(define-class Immutable-Point Object 
;  ((= x :immutable)
;   (= y :immutable) ) )
;   Immutable-Point
;(set! the-Point (make-Immutable-Point 2 44))
;   ---
;(Immutable-Point-x the-Point)
;   2
;(Immutable-Point-y the-Point)
;   44
;(Immutable-Point? the-Point)
;   #t
;(Object? the-Point)
;   #t
;(Class? the-Point)
;   #f
;(map Mutable-Mono-Field? (Class-fields (->Class 'Immutable-Point)))
;   (#f #f)
;;; Testing immutability conservation by inheritance
;(define-class Colored-Immutable-Point Immutable-Point
;  (color) )
;   Colored-Immutable-Point
;(map Mutable-Mono-Field? 
;     (Class-fields (->Class 'Colored-Immutable-Point)) )
;   (#f #f #t)

;;; Testing immutability of a class and carelessness
;(define-class Other-Immutable-Point Object 
;  (x y) :immutable :careless )
;   Other-Immutable-Point
;(set! the-Point (make-Other-Immutable-Point 2 44))
;   ---
;(Other-Immutable-Point-x the-Point)
;   2
;(Other-Immutable-Point-y the-Point)
;   44
;(Other-Immutable-Point? the-Point)
;   #t
;(Object? the-Point)
;   #t
;(Class? the-Point)
;   #f
;(map Mutable-Mono-Field? 
;     (Class-fields (->Class 'Other-Immutable-Point)) )
;   (#f #f)
;;; Testing immutability conservation by inheritance
;(define-class Colored-Other-Immutable-Point Other-Immutable-Point
;  (color) )
;   Colored-Other-Immutable-Point
;(map Mutable-Mono-Field? 
;     (Class-fields (->Class 'Colored-Other-Immutable-Point)) )
;   (#f #f #t)
;;; Testing Mutable-Field? generic function
;(map Mutable-Field? 
;     (Class-fields (->Class 'Other-Immutable-Point)) )
;   (#f #f)
;;; testing care(ful,less)
;(set! the-Point (make-Point 2 44))
;   ---
;(Point-x the-Point)
;   2
;(Other-Immutable-Point-x the-Point)
;   2
;(Colored-Other-Immutable-Point-x the-Point)
;   2
;(set! the-Point (make-Colored-Immutable-Point 2 44 'pink))
;   ---
;(Point-x the-Point)
;   ***
;(Other-Immutable-Point-x the-Point)
;   2
;(Colored-Other-Immutable-Point-x the-Point)
;   2
;; test on predefined classes
;(Class-name (make-Object))
;   ***

;;; Testing that :prototype does no harm (no more in Meroonet)
(define-generic (zztop (o)) 'default)
   ---
(define-method (zztop (o ColoredPoint)) 'ColoredPoint)
   ---
(set! the-Point (make-NamedColoredPoint 33 44 'red 2 'joe 'jill))
   ---
(zztop the-Point)
   ColoredPoint
(zztop (make-ColoredPoint 55 66 'blue))
   ColoredPoint
(zztop (make-Point 55 66))
   default
;(define-class Point Object (x y) :prototype)
;   ---
(zztop the-Point)
   ColoredPoint
(zztop (make-ColoredPoint 55 66 'blue))
   ColoredPoint
(zztop (make-Point 55 66))
   default
;(define-class ColoredPoint Point (color) :prototype)
;   ---
(zztop the-Point)
   ColoredPoint
(zztop (make-ColoredPoint 55 66 'blue))
   ColoredPoint
(zztop (make-Point 55 66))
   default
;(define-class NamedColoredPoint ColoredPoint ((* names)) :prototype)
;   NamedColoredPoint
(zztop the-Point)
   ColoredPoint
(zztop (make-ColoredPoint 55 66 'blue))
   ColoredPoint
(zztop (make-Point 55 66))
   default
; this cannot be done in interpreted mode.
;(define-class Point Object (x y z) :prototype) ;; one more field
;   ***
;(define-class Point Object (x) :prototype) ;; one field less
;   ***
;(define-class Point Object (x yy) :prototype) ;; one field name changed
;   ***


;;; testing show-generic-trace (No more in Meroonet)

;(show-generic-trace 'foo)
;   ---
;(foo (make-Point 1 2) 3)
;   (point)
;(foo (make-Point 1 2) 3 4)
;   (point 4)
;(foo (make-Point 1 2) 3 4 5 6)
;   (point 4 5 6)
;(show-generic-trace 'foo)
;   ---   ; just redefine foo to be traced exactly the same
;(foo (make-Point 1 2) 3)
;   (point)
;(foo (make-Point 1 2) 3 4)
;   (point 4)
;(foo (make-Point 1 2) 3 4 5 6)
;   (point 4 5 6)
;(show-generic-untrace 'foo)
;   ---  ; no more trace
;(foo (make-Point 1 2) 3)
;   (point)
;(foo (make-Point 1 2) 3 4)
;   (point 4)
;(foo (make-Point 1 2) 3 4 5 6)
;   (point 4 5 6)
;(show-generic-untrace 'foo)
;   ---
;(foo (make-Point 1 2) 3)
;   (point)
;(foo (make-Point 1 2) 3 4)
;   (point 4)
;(foo (make-Point 1 2) 3 4 5 6)
;   (point 4 5 6)

;;; test a class without slots
(define-class X Object ())
   ---
(make-X)
   ---
(X? (make-X))
   #t
(define-class X-Point Point ())
   ---
(set! the-Point (make-X-Point 11 22))
   ---
(X-Point-x the-Point)
   11
(X-Point? the-Point)
   #t
(X? the-Point)
   #f


;;; Testing exceptions
;(define-class 3 Object ())
;   ***
;;;;(define-class X 3 ())
;;;;   ***
;;;;(define-class X Unknown ())
;;;;   ***
;(define-class Anomaly Object ())
;   ***
;(define-class Anomaly Anomaly ())
;   ***
;(allocate-NamedColoredPoint 1 -3 3)
;   ***
;;;;(allocate-NamedColoredPoint 'foo)
;;;;   ***
;;;;(allocate-NamedColoredPoint -2)
;;;;   ***
;;;;(allocate-NamedColoredPoint)
;;;;   ***
;(make-Point 11 22 33)
;   ***
;;;;(make-Point)
;;;;   ***
;;;;(make-Point 11)
;;;;   ***
;(make-NamedColoredPoint 11 22 'red 0 9)
;   ***
;(make-NamedColoredPoint 11 22 'red 0 9 8)
;   ***
;(make-NamedColoredPoint 11 22 'red 1 9 8)
;   ***
;;;;(make-NamedColoredPoint 11 22 'red 'foo 9)
;;;;   ***
;(make-NamedColoredPoint 11 22 'red -3 9)
;   ***
;;;;(define-class Y Point (x))
;;;;   ***
;;;;(define-class Y Point ((* x)))
;;;;   ***
;;;;(define-class Y Object (3))
;;;;   ***
;;;;(define-class Y Object ((= 3)))
;;;;   ***
;(define-class Y Object ((= foo :bla-bla)))        ; Not an error
;   ---
;;;;(ColoredPoint-color (make-Point 111 222))
;;;;   ***
;;;;(set-ColoredPoint-color! (make-Point 111 222) 'red)
;;;;   ***
;(->Class 3)
;   ***
;(->Generic 4)
;   ***
;;;;(define-generic (zz))
;;;;   ***
;;;;(define-generic (zz a b . c))
;;;;   ***
;;;;(define-generic foo (a b))
;;;;   ***
;;;;(define-method foo)
;;;;   ***
;;;;(define-method foo a)
;;;;   ***
(define-generic (zz a (o) . c))
   ---
;;;;(define-method (zz (a)))
;;;;   ***
;;;;(define-method (zz (a) b))
;;;;   ***
;;;;(define-method (zz (a) c . b))
;;;;   ***
;;;;(define-method (zz s (a) b))
;;;;   ***
;;;;(define-method (zz (a Point)))
;;;;   ***
;;;;(define-method (zz (a Point) b))
;;;;   ***
;;;;(define-method (zz (a Point) c . b))
;;;;   ***
;;;;(define-method (zz s (a Point) b))
;;;;   ***
;;;;(define-method (yy (a Class)) 4)
;;;;   ***
;;;;(define-method (clone (a ZZZ)) 5)
;;;;   ***
;(show-hierarchy 3)
;   ***
;(show-generic 3)
;   ***
;(generic-trace 33 car car)
;   ***
;(generic-trace zz 2 car)
;   ***
;(generic-trace zz car cdr)
;   ***
;(generic-untrace 3)
;   ***
;(show-generic-trace 44)
;   ***
;(show-generic-untrace 44)
;   ***

;;; Test metaclass
;(define-class Counting-Class Class (counter))
;   ---
;(define-method (initialize! (o Counting-Class))
;  (call-next-method)
;  (set-Counting-Class-counter! o 0)
;  (let ((original-maker (Class-maker o)))
;    (set-Counting-Class-maker! 
;     o (lambda args
;         (set-Counting-Class-counter! o (+ 1 (Counting-Class-counter o))) 
;         (apply original-maker args) ) ) )
;  o )
;   ---
;(define-class Counted-Point Point () :metaclass Counting-Class)
;   ---
;(->Class 'Counted-Point)
;   ---
;(Counting-Class-counter (->Class 'Counted-Point))
;   0
;(set! pt (make-Counted-Point 22 33))
;   ---
;(Counted-Point-x pt)
;   22
;(Point-y pt)
;   33
;(Counting-Class-counter (->Class 'Counted-Point))
;   1

;;;testing :eponymous
;(define-class foo Object (x) :eponymous)
;   ---
;(eq? foo (->Class 'foo))
;   #t

;;; Testing show-unveiled
;(unveil (->Class 'Mutable-Poly-Field))
;   ---
;;; testing unveil with circularity
;(define-class A Object (x))
;   ---
;(define-class B Object (y))
;   ---
;(begin (set! the-Point (make-A (make-B 'wait)))
;       (set-B-y! (A-x the-Point) the-Point)
;       (unveil the-Point) )
;   ---
;(define-class C A ((* z)))
;   ---
;(begin (set! the-Point (make-C (make-B 'wait)
;                               3 (make-C 11 0) 
;                                 (make-B 22)
;                                 (make-B 33) ))
;       (set-B-y! (A-x the-Point) the-Point)
;       (set-B-y! (C-z the-Point 1) (C-z the-Point 2))
;       (unveil the-Point) )
;   ---

;;; testing field-value (these tests come from Meroon-V3)
(define-class A Object (x))
   ---
(define-class C A (y (* z)))
   ---
(define-class D C (t))
   ---
(define-class E D ((* u)))
   ---
(define-class F E (v))
   ---
(set! the-Point (make-F 'xx 'yy 3 'zz0 'zz1 'zz2 'tt 4 'u0 'u1 'u2 'u3 'vv))
   ---
(let ((f (list-ref (Class-fields (->Class 'A)) 0)))
  (field-value the-Point f) )
   xx
(let ((f (list-ref (Class-fields (->Class 'C)) 0)))
  (field-value the-Point f) )
   xx
(let ((f (list-ref (Class-fields (->Class 'C)) 1)))
  (field-value the-Point f) )
   yy
(let ((f (list-ref (Class-fields (->Class 'C)) 2)))
  (field-value the-Point f 0) )
   zz0
(let ((f (list-ref (Class-fields (->Class 'C)) 2)))
  (field-value the-Point f 2) )
   zz2
(let ((f (list-ref (Class-fields (->Class 'D)) 2)))
  (field-value the-Point f 2) )
   zz2
(let ((f (list-ref (Class-fields (->Class 'F)) 2)))
  (field-value the-Point f 2) )
   zz2
(let ((f (list-ref (Class-fields (->Class 'D)) 3)))
  (field-value the-Point f) )
   tt
(let ((f (list-ref (Class-fields (->Class 'F)) 4)))
  (field-value the-Point f 2) )
   u2
(let ((f (list-ref (Class-fields (->Class 'F)) 5)))
  (field-value the-Point f) )
   vv
;;;;;;;;;;;;;;;;;;;;;;;;;;                        testing set-field-value
;(let ((f (list-ref (Class-fields (->Class 'A)) 0)))
;  (set-field-value! the-Point 'xxx f)
;  (field-value the-Point f) )
;   *** ; immutable field
;(let ((f (list-ref (Class-fields (->Class 'C)) 0)))
;  (set-field-value! the-Point 'xxxx f)
;  (field-value the-Point f) )
;   *** ; immutable field by inheritance
(let ((f (list-ref (Class-fields (->Class 'C)) 1)))
  (set-field-value! the-Point 'yyy f)
  (field-value the-Point f) )
   yyy
(let ((f (list-ref (Class-fields (->Class 'C)) 2)))
  (set-field-value! the-Point 'zzz0 f 0)
  (field-value the-Point f 0) )
   zzz0
(let ((f (list-ref (Class-fields (->Class 'C)) 2)))
  (set-field-value! the-Point 'zzz2 f 2)
  (field-value the-Point f 2) )
   zzz2
(let ((f (list-ref (Class-fields (->Class 'D)) 2)))
  (set-field-value! the-Point 'zzz22 f 2)
  (field-value the-Point f 2) )
   zzz22
(let ((f (list-ref (Class-fields (->Class 'F)) 2)))
  (set-field-value! the-Point 'zzz222 f 2)
  (field-value the-Point f 2) )
   zzz222
(let ((f (list-ref (Class-fields (->Class 'D)) 3)))
  (set-field-value! the-Point 'ttt f)
  (field-value the-Point f) )
   ttt
(let ((f (list-ref (Class-fields (->Class 'F)) 4)))
  (set-field-value! the-Point 'uu2 f 2)
  (field-value the-Point f 2) )
   uu2
(let ((f (list-ref (Class-fields (->Class 'F)) 5)))
  (set-field-value! the-Point 'vvv f)
  (field-value the-Point f) )
   vvv
;;;;;;;;;;;;;;;;;;;;;;; testing field-length
(let ((f (list-ref (Class-fields (->Class 'F)) 2)))
  (field-length the-Point f) )
   3
(let ((f (list-ref (Class-fields (->Class 'F)) 4)))
  (field-length the-Point f) )
   4
; not detected in Meroonet
;(let ((f (list-ref (Class-fields (->Class 'F)) 3)))
;  (field-length the-Point f) )
;   *** ; not a poly-field

;;; end of oo-tmp.scm
