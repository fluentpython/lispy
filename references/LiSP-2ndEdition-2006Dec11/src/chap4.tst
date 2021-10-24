;;; Testing functions of chapter 4

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

(min-max '1)
   (1 1)
(min-max '(1 . 2))
   (1 2)
(min-max '((1 . 2) 3 4 ((5 . 6) . 7) . 8))
   (1 8)

(min-max1 '1)
   (1 1)
(min-max1 '(1 . 2))
   (1 2)
(min-max1 '((1 . 2) 3 4 ((5 . 6) . 7) . 8))
   (1 8)

(min-max2 '1)
   (1 1)
(min-max2 '(1 . 2))
   (1 2)
(min-max2 '((1 . 2) 3 4 ((5 . 6) . 7) . 8))
   (1 8)

(enumerate)
   0
(enumerate)
   1
(enumerate)
   2

(let ((name "Nemo"))
  (set! winner (lambda () name))
  (set! set-winner! (lambda (new-name) 
                      (set! name new-name) name )) )
   ---
(winner)
  "Nemo"
(set-winner! "Me")
  "Me"
(winner)
  "Me"

(let ((name "Nemo"))
  (set! winner (lambda () name)) )
   ---
(let ((name "Nemo"))
  (set! set-winner! (lambda (new-name) (set! name new-name)
                                       name )) ) 
   ---
(winner)
  "Nemo"
(set-winner! "Me")
  "Me"
(winner)
  "Nemo"

(set! box1 (make-box 33))
   ---
(box-ref box1)
   33
(box-set! box1 44)
   ---
(box-ref box1)
   44

(set! box1 (other-make-box 33))
   ---
(other-box-ref box1)
   33
(other-box-set! box1 44)
   ---
(other-box-ref box1)
   44

(let ((name (make-box "Nemo")))
  (set! winner (lambda () (box-ref name)))
  (set! set-winner! (lambda (new-name) (box-set! name new-name)
                                       (box-ref name) )) ) 
   ---
(winner)
   "Nemo"
(set-winner! "Me") 
   "Me"
(winner)
   "Me"


(set! p1 (kons 'a 'b))
   ---
(kar p1)
   a
(kdr p1)
   b
(begin (set-kar! p1 'c) (kar p1))
   c
(begin (set-kdr! p1 'd) (kdr p1))
   d

(set! p1 (make-named-box 'joe 33))
   ---
((p1 'type))
   named-box
((p1 'name))
   joe
((p1 'ref))
   33
((p1 'set!) 44)
   ---
((p1 'ref))
   44
(eqv? (p1 'type) (p1 'type))
   #f

(set! p1 (other-make-named-box 'joe 33))
   ---
((p1 'type))
   named-box
((p1 'name))
   joe
((p1 'ref))
   33
((p1 'set!) 44)
   ---
((p1 'ref))
   44
(eqv? (p1 'type) (p1 'type))
   #t

;;; Redefine memq so that errors are reported.
(define (memq a l)
 (if (pair? l)
     (if (eq? a (car l))
         l
         (memq a (cdr l)) )
     #f ) )
   ---
(vowel<= #\o #\u)
   (#\u)
(set-cdr! (vowel<= #\a #\e) '())
   ---
(vowel<= #\o #\u)
   #f

(vowel1<= #\o #\u)
   #f
(vowel1<= #\e #\i)
   #f
(set-cdr! (vowel1<= #\a #\e) '(#\i))
   ---
(vowel1<= #\o #\u)
   #f
(vowel1<= #\e #\i)
   (#\i)

(vowel2<= #\o #\u)
   (#\u)
(set-cdr! (vowel2<= #\a #\e) '())
   ---
(vowel2<= #\o #\u)
   (#\u)
(eq? (cdr (vowel2<= #\a #\e)) (vowel2<= #\e #\i))
   #f

(vowel3<= #\o #\u)
   (#\u)
(set-cdr! (vowel3<= #\a #\e) '())
   ---
(vowel3<= #\o #\u)
   (#\u)
(eq? (cdr (vowel3<= #\i #\o)) (vowel3<= #\o #\u))
   #t


(define bar (cycle 0 1))
   ---
(eq? bar (cddr bar))
   #t


(pair-eq? (cons 'a 'b) (cons 'a 'b))
   #f
(set! p1 (cons 'a 'b))
   ---
(pair-eq? p1 p1)
   #t
p1 
   (a . b)

(qar (qons 1 2))
   1
(qdr (qons 3 4))
   4
(qar (qdr (qons 1 (qons 2 3))))
   2

