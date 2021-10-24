;;; $Id: chap8i.tst,v 4.0 1995/07/10 06:52:10 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Testing the import special form

;;; Not using imported variables
((lambda (a)
   (import () (export) a) )
 2 )
   2
((lambda (a b)
   (import () (export) a b) )
 2 3 )
   3
((lambda (a b)
   (import (a) (export) b) )
 2 3 )
   3
((lambda (f b c)
    (import (a) (procedure->environment f) c) )
 ((lambda (a b) (lambda (x) (list a x)))
  2 3 )
 4 
 5 )
   5
((lambda (f b c)
    (import (a) (procedure->environment f) b) )
 ((lambda (a b) (lambda (x) (list a x)))
  2 3 )
 4 
 5 )
   4
((lambda (f b c)
    (import (a) (procedure->environment f) 
       ((lambda (a) a)
        b ) ) )
 ((lambda (a b) (lambda (x) (list a x)))
  2 3 )
 4 
 5 )
   4


;;; Uses now imported variables
;;; The next one does not loop since (export) is computed as if
;;; external to import.
((lambda (a b)
   (import (a) (export) a) )
 2 3 )
   2
((lambda (a b)
   (import (a) (enrich (export) 'a) a) )
 2 3 )
   *** ; a uninitialized
((lambda (a b)
   (import (a) (enrich (export) 'a) a b) )
 2 3 )
   *** ; a uninitialized even if many optimizations removes the reference to a
((lambda (f b c)
    (import (a) (procedure->environment f) a) )
 ((lambda (a b) (lambda (x) (list a x)))
  2 3 )
 4 
 5 )
   2
;;; capture a global binding
((lambda (e) 
   ((lambda (car) (import (car) e (car (cons 1 2))))
    cdr ) )
 (export car) )
    1
;;; capture a shadowed local
((lambda (f x y)
    (import (c) (f x) c) )
 ((lambda (a b) (lambda (e) (import (c) e (export a b c))))
  1 2 )
 ((lambda (c) (export)) 5)
 4 )
   5
((lambda (f x y)
    (import (c a) (f x) (list a c)) )
 ((lambda (a b) (lambda (e) (import (c) e (export a b c))))
  1 2 )
 ((lambda (c) (export)) 5)
 4 )
   (1 5)

;;; assignment
((lambda (a b)
   (import (a) (export) (set! a b) a) )
 2 3 )
   3
((lambda (a b)
   (import (a) (enrich (export) 'a) (set! a b) a) )
 2 3 )
   3
((lambda (f b c)
    (import (a) (procedure->environment f) (set! a b))
    (f 6) )
 ((lambda (a b) (lambda (x) (list a x)))
  2 3 )
 4 
 5 )
   (4 6)
;;; capture a global binding
((lambda (e) 
   ((lambda (foo) (import (foo) e (set! foo (cons 1 2))))
    cdr )
   foo )
 (export foo) )
    (1 . 2)
;;; capture a shadowed local
((lambda (f x y)
    (import (c) (f x) (set! c 7))
    (import (c) x c) )
 ((lambda (a b) (lambda (e) (import (c) e (export a b c))))
  1 2 )
 ((lambda (c) (export)) 5)
 4 )
   7

;;; Testing the defined? function. It rather tests that a variable has
;;; a value rather than its existence (since to mention it makes it alive).
((lambda (defined?) (defined? (export) 'defined?))
 (lambda (env name)
    (bind-exit (return)
      (monitor (lambda (c ex) (return #f))
         (eval/b name env)
         #t ) ) ) )
  #t
((lambda (defined?) (defined? (export) 'car))
 (lambda (env name)
    (bind-exit (return)
      (monitor (lambda (c ex) (return #f))
         (eval/b name env)
         #t ) ) ) )
  #t
((lambda (defined?) (defined? (export) 'xyzzy))
 (lambda (env name)
    (bind-exit (return)
      (monitor (lambda (c ex) (return #f))
         (eval/b name env)
         #t ) ) ) )
  #f

;;; end of chap8i.tst
