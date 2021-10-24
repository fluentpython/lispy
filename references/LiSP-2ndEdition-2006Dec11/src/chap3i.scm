;;; $Id$

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

(define (fact n k)
  (if (= n 1) (k 1)
      (fact (- n 1) (lambda (r) (k (* n r)))) ) )

(define (divide p q f)
  (f (quotient p q) (remainder p q)) )

(define (bezout n p k)    ; assume $n>p$
  (divide 
   n p (lambda (q r)
         (if (= r 0)
             (if (= p 1)
                 (k 0 1)  ; since $ 0 \times 1 - 1 \times 0 = 1 $
                 (error "not relatively prime" n p) )
             (bezout
              p r (lambda (u v)
                    (k v (- u (* v q))) ) ) ) ) ) )
;;; (bezout 45 5 list)
;;; (bezout 45 53 list)
;;; (bezout 1991 1960 list)

(define (cc f)
  (let ((reified? #f))
    (let ((k (the-current-continuation)))
      (if reified? k (begin (set! reified? #t) (f k))) ) ) )


(define (cps-fact n k)
  (if (= n 0) (k 1) (cps-fact (- n 1) (lambda (v) (k (* n v))))) )

(define (make-box value)
  (let ((box
         (call/cc
          (lambda (exit)
            (letrec
               ((behavior
                 (call/cc
                  (lambda (store)
                    (exit (lambda (msg . new)
                            (call/cc
                             (lambda (caller)
                               (case msg
                                 ((get) (store (cons (car behavior)
                                                     caller )))
                                 ((set)
                                  (store 
                                   (cons (car new) 
                                         caller ) ) ) ) ) ) )) ) ) ))
              ((cdr behavior) (car behavior)) ) ) ) ) )
    (box 'set value)
    box ) )

;;; end of chap3i.scm
