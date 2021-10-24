;;; $Id: chap5e.scm,v 4.0 1995/07/10 06:51:28 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; Denotation preserving the unspecification of the evaluation order

;;; Forall cuts the list L in all possible ways and applies F on the
;;; three obtained parts. 
;;; forall : ((a list * a * a list) -> b list) * (a list) -> b list
(define (forall f l)
  ;; (assume (pair? l))
  (letrec ((loop (lambda (before item after)
                   (union (f before item after)
                          (if (pair? after)
                              (loop (append before (list item))
                                    (car after)
                                    (cdr after) ) 
                              empty-set ) ) )))
    (loop (list) (car l) (cdr l)) ) )
;(forall list '(a b c d))

;;; Set representation
(define union append)
(define empty-set '())

;;; Invoke q on V* cut into two parts, the first I items and the rest.
(define (cut i v* q)
  (letrec ((accumulate 
            (lambda (left i right)
              (if (> i 0)
                  (accumulate (cons (car right) left)
                              (- i 1)
                              (cdr right) )
                  (q (reverse left) right) )) ))
    (accumulate (list) i v*) ) )
;(cut 3 '(a b c d e f g h i) list)

;;; Takes a list of denotations, builds all possible ways to interleave 
;;; them and returns the list of all possible answers.
;;; possible-path : meaning list -> env * cont * store -> Answer list
(define (possible-paths m+)
  (lambda (r k s)
    (if (pair? (cdr m+))
        (forall (lambda (<m m m>)
                  (m r
                     (lambda (v ss)
                       ((possible-paths (append <m m>))
                        r 
                        (lambda (v* sss)
                          (let ((q (lambda (<v v>)
                                     (k (append <v (list v) v>) sss) )))
                            (cut (length <m) v* q) ) )
                        ss ) )
                     s ) )
                m+ )
        ((car m+) r 
                  (lambda (v ss) (k (list v) ss))
                  s ) ) ) )

;;; The denotation of an application. E+ represents all the terms of an
;;; application. As usual R is the lexical environment, K the continuation
;;; and S the store.
(define ((meaning-orderless-application *e*0*n) r k s)
  ((possible-paths (map meaning *e*0*n))
   r 
   (lambda (v* s)
     ((Value->Function (car v*)) (cdr v*) k s) )
   s ) )

(set! meaning-application 
      (lambda (e e*) (meaning-orderless-application (cons e e*))) )

(define ((new-meaning e*0*n) r k s)
  (oneof ((meaning e*0*n) r k s)) )

;;; Tests

(define (den+Scheme)
  (interpreter 
   "den+Scheme? "
   "den+Scheme= "
   #f
   (lambda (read print error)
     (set! wrong error)
     (let ((s.current s.init))
       (lambda ()
         (let ((e (read)))
           (print ((meaning e)
                   r.init
                   (lambda (v s.final)
                     (set! s.current s.final)
                     (let ((v.final (convert v s.final)))
                       (display "== ")
                       (display v.final)
                       (newline)
                       (list v.final) ) )
                   s.current )) ) ) ) ) ) )

(define (test-den+Scheme file)
  (suite-test 
   file
   "den+Scheme? " 
   "den+Scheme= " 
   #t
   (lambda (read check error)
     (set! wrong error)
     (let ((s.current s.init))
       (lambda ()
         (let ((results ((meaning (read))
                         r.init
                         (lambda (v s.final)
                           (set! s.current s.final)
                           (let ((v.final (convert v s.final)))
                             (display "== ")
                             (display v.final)
                             (newline)
                             (list v.final) ) )
                         s.current )))
           (check results) ) ) ) )
   set-equal? ) )

;;; end of chap5e.scm
