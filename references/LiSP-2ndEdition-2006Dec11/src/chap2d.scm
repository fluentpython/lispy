;;; $Id: chap2d.scm,v 4.1 2006/11/27 14:02:27 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;;; Excerpts from chapter 2. This is Scheme code.

(define (display-cyclic-spine list)
  (define (scan l1 l2 flip)
    (cond ((atom? l1)  (unless (null? l1) (display " . ")
                              (display l1) )
                       (display ")") )
          ((eq? l1 l2) (display "...)") )
          (else        (display (car l1))
                       (when (pair? (cdr l1)) (display " "))
                       (scan (cdr l1) 
                             (if (and flip (pair? l2)) (cdr l2) l2)
                             (not flip) ) ) ) )
  (display "(")
  (scan list (cons 123 list) #f) )

(display-cyclic-spine                    ; prints {\tt(1 2 3 4 1 ...)}
 (let ((l (list 1 2 3 4)))
   (set-cdr! (cdddr l) l)
   l ) )

(define (fact n)
  (if (= n 0) 1
      (* n (fact (- n 1))) ) )

(or (= (fact 5) 120)
    this-would-be-an-error )

;;; recursion combinator

(let ((W (lambda (w)
           (lambda (f) 
             (f ((w w) f)) ) )))
   (W W) ) 

(define fix
  (let ((d (lambda (w) 
             (lambda (f) 
               (f (lambda (x) (((w w) f) x))) ) )))
    (d d) ) )

(define (meta-fact f)
  (lambda (n)
     (if (= n 0) 1
         (* n (f (- n 1))) ) ) )

(or (= ((fix meta-fact) 5) 120)
    this-would-be-an-error )

(define odd-and-even
  (fix (lambda (f)
         (lambda (which)
           (case which
             ((odd) (lambda (n) (if (= n 0) #f
                                    ((f 'even) (- n 1)) )))
             ((even) (lambda (n) (if (= n 0) #t
                                     ((f 'odd) (- n 1)) ))) ) ) )) )

(define odd? (odd-and-even 'odd))
(define even? (odd-and-even 'even))

(or (and (odd? 5)
         (not (odd? 4))
         (even? 4)
         (not (even? 5)) )
    this-would-be-an-error )

(define fix2
  (let ((d (lambda (w) 
             (lambda (f)
               (f (lambda (x y) (((w w) f) x y))) ) )))
    (d d) ) )

(or (equal? ((fix2 (lambda (append)
                     (lambda (x y)
                       (if (pair? x)
                           (cons (car x) (append (cdr x) y))
                           y ) ) ))
             '(1 2 3) '(4 5 6) )
            '(1 2 3 4 5 6) )
    this-would-be-an-error )

(define fixN
  (let ((d (lambda (w) 
             (lambda (f)
               (f (lambda args (apply ((w w) f) args))) ) )))
    (d d) ) )

(or (equal? ((fixN (lambda (append)
                     (lambda (x y)
                       (if (pair? x)
                           (cons (car x) (append (cdr x) y))
                           y ) ) ))
             '(1 2 3) '(4 5 6) )
            '(1 2 3 4 5 6) )
    this-would-be-an-error )

(define NfixN
  (let ((d (lambda (w)
             (lambda (f*)
               (list ((car f*) 
                      (lambda a (apply (car ((w w) f*)) a))
                      (lambda a (apply (cadr ((w w) f*)) a)) )
                     ((cadr f*)
                      (lambda a (apply (car ((w w) f*)) a))
                      (lambda a (apply (cadr ((w w) f*)) a)) ) ) ) )))
    (d d) ) )

(or (equal?
     (let ((odd-and-even (NfixN (list (lambda (odd? even?)    ;\relax{\tt odd?}
                                        (lambda (n)
                                          (if (= n 0) #f (even? (- n 1))) ) )
                                      (lambda (odd? even?)    ;\relax{\tt even?}
                                        (lambda (n)
                                          (if (= n 0) #t (odd? (- n 1))) ) ) ))))
       (set! odd? (car odd-and-even))
       (set! even? (cadr odd-and-even))
       (list (odd? 5) (odd? 4) (even? 5) (even? 4)) )
     '(#t #f #f #t) )
    this-would-be-an-error )

(define NfixN2
  (let ((d (lambda (w)
             (lambda (f*)
               (map (lambda (f) 
                      (apply f (map (lambda (i) 
                                      (lambda a (apply 
                                                 (list-ref ((w w) f*) 
                                                           i ) 
                                                 a )) )
                                    (iota 0 (length f*)) )) )
                    f* ) ) )))
    (d d) ) )

(or (equal?
     (let ((odd-and-even (NfixN2 (list (lambda (odd? even?)    ;\relax{\tt odd?}
                                        (lambda (n)
                                          (if (= n 0) #f (even? (- n 1))) ) )
                                      (lambda (odd? even?)    ;\relax{\tt even?}
                                        (lambda (n)
                                          (if (= n 0) #t (odd? (- n 1))) ) ) ))))
       (set! odd? (car odd-and-even))
       (set! even? (cadr odd-and-even))
       (list (odd? 5) (odd? 4) (even? 5) (even? 4)) )
     '(#t #f #f #t) )
    this-would-be-an-error )

(define klop
  (let ((r (lambda (s c h e m)
             (lambda (f)
               (f (lambda (n) 
                    (((m e c h e s) f) n) )) ) )))
    (r r r r r r) ) )

(or (= ((klop meta-fact) 5) 120)
    this-would-be-an-error )

(define (hyper-fact f)
  (lambda (n)
    (if (= n 0) 1
        (* n ((f f) (- n 1))) ) ) )

(or (= ((hyper-fact hyper-fact) 5) 120)
    this-would-be-an-error )


(define (factfact n)
  (define (internal-fact f n)
    (if (= n 1) 1
        (* n (f f (- n 1))) ) )
  (internal-fact internal-fact n) )

(or (= 120 (factfact 5))
    this-would-be-an-error )


;;; exercice

(define-syntax dynamic-let
  (syntax-rules ()
    ((dynamic-let () . body)
     (begin . body) )
    ((dynamic-let ((variable value) others ...) . body)
     (bind/de 'variable (list value)
              (lambda () (dynamic-let (others ...) . body)) ) ) ) )
(define-syntax dynamic
  (syntax-rules ()
    ((dynamic variable)
     (car (assoc/de 'variable specific-error)) ) ) )
(define-syntax dynamic-set!
  (syntax-rules ()
    ((dynamic-set! variable value)
     (set-car! (assoc/de 'variable specific-error) value) ) ) ) 

;;; Exercice on Plists

(let ((properties '()))
  (set! putprop 
        (lambda (symbol key value)
          (let ((plist (assq symbol properties)))
            (if (pair? plist)
                (let ((couple (assq key (cdr plist))))
                  (if (pair? couple)
                      (set-cdr! couple value)
                      (set-cdr! plist (cons (cons key value) 
                                            (cdr plist) )) ) )
                (let ((plist (list symbol (cons key value))))
                  (set! properties (cons plist properties)) ) ) )
          value ) )
  (set! getprop
        (lambda (symbol key)
          (let ((plist (assq symbol properties)))
            (if (pair? plist)
                (let ((couple (assq key (cdr plist))))
                  (if (pair? couple)
                      (cdr couple)
                      #f ) )
                #f ) ) ) ) ) 

(or (begin (putprop 'symbol 'key 'value)
           (eq? (getprop 'symbol 'key) 'value) )
    this-would-be-an-error )

(or (not (getprop 'symbol 'lad))
    this-would-be-an-error )

;;; end of chap2d.scm
