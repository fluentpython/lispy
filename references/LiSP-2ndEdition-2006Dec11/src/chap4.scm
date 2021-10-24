;;; $Id: chap4.scm,v 4.2 1996/01/14 14:14:29 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; Programs of chapter 4.

;;; Variables defined below:
(define can-access? 'void)

(define (min-max tree)
  (define (first-number tree)
    (if (pair? tree)
        (first-number (car tree))
        tree ) )
  (let* ((min (first-number tree))
         (max min) )
    (define (scan! tree)
      (cond ((pair? tree)
             (scan! (car tree))
             (scan! (cdr tree)) )
            (else (if (> tree max) (set! max tree)
                      (if (< tree min) (set! min tree)) )) ) )
    (scan! tree)
    (list min max) ) )


(define (min-max1 tree)
  (define (mm tree)
    (if (pair? tree)
        (let ((a (mm (car tree)))
              (d (mm (cdr tree))) )
          (list (min (car a) (car d))
                (max (cadr a) (cadr d)) ) )
        (list tree tree) ) )
  (mm tree) )

(define (min-max2 tree)
  (define (mm tree k)
    (if (pair? tree)
        (mm (car tree)
            (lambda (mina maxa)
              (mm (cdr tree)
                  (lambda (mind maxd)
                    (k (min mina mind)
                       (max maxa maxd) ) ) ) ) )
        (k tree tree) ) )
  (mm tree list) ) 

(define enumerate
  (let ((n -1))
    (lambda () (set! n (+ n 1))
               n ) ) )

(define winner      'sans-queue)
(define set-winner! 'ni-tete)

(let ((name "Nemo"))
  (set! winner (lambda () name))
  (set! set-winner! (lambda (new-name) (set! name new-name)
                                       name ))
  (set-winner! "Me")
  (winner) ) 

(let ((name "Nemo"))
  (set! winner (lambda () name))
  (winner) )

(let ((name "Nemo"))
  (set! set-winner! (lambda (new-name) (set! name new-name) 
                                       name ))
  (set-winner! "Me") ) 

(define (make-box value)
  (lambda (msg)
    (case msg
      ((get) value)
      ((set!) (lambda (new-value) (set! value new-value))) ) ) )

(define (box-ref box)
  (box 'get) )

(define (box-set! box new-value)
  ((box 'set!) new-value) ) 


(define (other-make-box value)
  (cons 'box value) )

(define (other-box-ref box)
  (cdr box) )

(define (other-box-set! box new-value)
  (set-cdr! box new-value) )

(define (crypt pw) \ldots)

(let ((passwd "timhukiTrolrk"))
  (set! can-access? (lambda (pw) (string=? passwd (crypt pw)))) )

(define (gatekeeper)
  (until (can-access? (read)) (gatekeeper)) )

(define (kons a d)
  (lambda (msg)
    (case msg
      ((car) a)
      ((cdr) d)
      ((set-car!) (lambda (new) (set! a new)))
      ((set-cdr!) (lambda (new) (set! d new))) ) ) )

(define (kar pair)
  (pair 'car) )

(define (kdr pair)
  (pair 'cdr) )

(define (set-kar! pair value)
  ((pair 'set-car!) value) ) 

(define (set-kdr! pair value)
  ((pair 'set-cdr!) value) ) 


(define (make-named-box name value)
  (lambda (msg)
    (case msg
      ((type) (lambda () 'named-box))
      ((name) (lambda () name))
      ((ref)  (lambda () value))
      ((set!) (lambda (new-value) (set! value new-value))) ) ) )

(define other-make-named-box
  (let ((type-closure (lambda () 'named-box)))
    (lambda (name value)
     (let ((name-closure  (lambda () name))
           (value-closure (lambda () value))
           (set-closure   (lambda (new-value) 
                            (set! value new-value) )) )
       (lambda (msg)
         (case msg
           ((type) type-closure)
           ((name) name-closure)
           ((ref)  value-closure)
           ((set!) set-closure) ) ) ) ) ) )

;;; quotations

(let ((foo (lambda () '(f o o))))
  (eq? (foo) (foo)) )

(define quote35 '(f o o))
(let ((foo (lambda () quote35)))
  (eq? (foo) (foo)) )

(define quote36 (cons 'o '()))
(define quote37 (cons 'o quote36))
(define quote38 (cons 'f quote37))
(let ((foo (lambda () quote38)))
  (eq? (foo) (foo)) )

(define symbol40 (string->symbol "f"))
(define symbol39 (string->symbol "o"))
(define quote36 (cons symbol39 '()))
(define quote37 (cons symbol39 quote36))
(define quote38 (cons symbol40 quote37))
(let ((foo (lambda () quote38)))
  (eq? (foo) (foo)) )

(define vowel<= 
  (let ((vowels '(#\a #\e #\i #\o #\u)))
    (lambda (c1 c2)
      (memq c2 (memq c1 vowels)) ) ) )

(define vowel1<= 
  (let ((vowels '(#\a #\e)))
    (lambda (c1 c2)
      (memq c2 (memq c1 vowels)) ) ) )

(define vowel2<= 
  (lambda (c1 c2)
    (case c1
     ((#\a) (memq c2 '(#\a #\e #\i #\o #\u)))
     ((#\e) (memq c2 '(#\e #\i #\o #\u)))
     ((#\i) (memq c2 '(#\i #\o #\u)))
     ((#\o) (memq c2 '(#\o #\u)))
     ((#\u) (eq? c2 #\u))
     (else  #f) ) ) )

(define quote82 (cons #\u '()))
(define quote81 (cons #\o quote82))
(define quote80 (cons #\i quote81))
(define quote79 (cons #\e quote80))
(define quote78 (cons #\a quote79))
(define vowel3<= 
  (lambda (c1 c2)
    (case c1
     ((#\a) (memq c2 quote78))
     ((#\e) (memq c2 quote79))
     ((#\i) (memq c2 quote80))
     ((#\o) (memq c2 quote81))
     ((#\u) (eq? c2 #\u))
     (else  #f) ) ) )


(define-syntax cycle
  (syntax-rules ()
    ((cycle terms ...)
     (let ((c (list terms ...)))
       (set-cdr! (last-pair c) c)
       c ) ) ) )

;;; Exercices

(define (pair-eq? a b)
  (let ((tag (list 'tag))
        (original-car (car a)) )
    (set-car! a tag)
    (let ((result (eq? (car b) tag)))
      (set-car! a original-car)
      result ) ) )

(define (new-evaluate e r s k)
  (if (atom? e) 
      (if (symbol? e) (evaluate-variable e r s k)
          (evaluate-quote e r s k) )
      (case (car e)
        ((quote)  (evaluate-quote (cadr e) r s k))
        ((if)     (evaluate-if (cadr e) (caddr e) (cadddr e) r s k))
        ((begin)  (evaluate-begin (cdr e) r s k))
        ((set!)   (evaluate-set! (cadr e) (caddr e) r s k))
        ((lambda) (evaluate-lambda (cadr e) (cddr e) r s k))
        ((or)     (evaluate-or (cadr e) (caddr e) r s k))             ; \modified
        (else     (evaluate-application (car e) (cdr e) r s k)) ) ) )

(define (evaluate-or e1 e2 r s k)
  (evaluate e1 r s (lambda (v ss)
                     (((v 'boolify)
                       (lambda () (k v ss))
                       (lambda () (evaluate e2 r s k)) ) ) )) )

(define (qons a d) (lambda (msg) (msg a d)))

(define (qar pair) (pair (lambda (a d) a))) 

(define (qdr pair) (pair (lambda (a d) d)))




;;; end of chap4.scm
