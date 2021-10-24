;;; $Id: chap6b.scm,v 4.0 1995/07/10 06:51:35 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;;                      Threaded interpreter.
;;; Dynamic creation of global variables as soon as there are seen.
;;; In fact they are not created at read-time but at compile-time and
;;; before evaluation-time. The problem is that at compile-time, we
;;; must compile (sic) so we must associate an index to any global
;;; variable so this index must be known by the compiler so the
;;; variable must be created by the compiler. 

(define (compute-kind r n)
  (or (local-variable? r 0 n)
      (global-variable? g.current n)
      (global-variable? g.init n)
      (adjoin-global-variable! n) ) )

;;; Do not check if the global environment is large enough.
;;; The new variable is stored at the beginning of sg.current.

(define (adjoin-global-variable! name)
  (let ((index (g.current-extend! name)))
    (vector-set! sg.current index undefined-value)
    (cdr (car g.current)) ) )

;;; Preserve the current modifiable global environment (containing a,
;;; b, foo, fact, fib etc.) All tests will be compiled in that environment.

(define original.g.current
  (let ((g g.current))
    (lambda () g) ) )

;;; Compile a program into a stand-alone program. It will initialize
;;; the global modifiable environment before starting evaluation.

(define (stand-alone-producer e)
  (set! g.current (original.g.current))
  (let* ((m (meaning e r.init))
         (size (length g.current)) )
    (lambda (sr k)
      (set! sg.current (make-vector size undefined-value))
      (m sr k) ) ) )

;;; Testing.
;;; We must skip the tests that presuppose a static non extensible
;;; global environment. Some of them appear in scheme-tests.scm.

(define (test-scheme6b file)
  (suite-test 
   file 
   "Scheme? " 
   "Scheme= "
   #t
   (lambda (read check error)
     (define (skip-read)
       (let ((e (read)))
         (if (member e *tests-to-skip*)
             (begin (read)              ; skip the associated result
                    (skip-read) )
             e ) ) )
     (set! wrong error)
     (set! static-wrong error)
     (lambda ()
       ((stand-alone-producer (skip-read)) sr.init check) ) )
   equal? ) )

(define *tests-to-skip*
  '( xyzzy
     (set! xyzzy 3)
     ((lambda (x y) xyzzy) 1 2)
    ) )
                
;;; end of chap6b.scm
