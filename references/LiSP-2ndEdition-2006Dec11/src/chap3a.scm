;;; $Id: chap3a.scm,v 4.3 2006/11/13 11:45:52 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Excerpts from chapter 3 (not necessarily Scheme)

(defun fact (n)                                  \[\em\hfill\cl\]
  (prog (r)
            (setq r 1)
       loop (cond ((= n 1) (return r)))
            (setq r (* n r))
            (setq n (- n 1))
            (go loop) ) )

(defun fact2 (n)                                  \[\em\hfill\cl\]
  (prog (r)
            (setq r 1)
       loop (setq r (* (cond ((= n 1) (return r))
                             ('else n) )
                       r ))
            (setq n (- n 1))
            (go loop) ) )

(define *active-catchers* '())

(define-syntax throw 
  (syntax-rules ()
    ((throw tag value)
     (let* ((label tag)                ; compute once
            (escape (assv label        ; compare with {\tt eqv?}
                          *active-catchers* )) )
       (if (pair? escape)
           ((cdr escape) value)
           (wrong "No associated catch to" label) ) ) ) ) )

(define-syntax catch 
  (syntax-rules ()
    ((catch tag . body)
     (let* ((saved-catchers *active-catchers*)
            (result (block label
                      (set! *active-catchers* 
                            (cons (cons tag 
                                        (lambda (x) 
                                          (return-from label x) ) )
                                  *active-catchers* ) )
                      . body )) )
       (set! *active-catchers* saved-catchers)
       result ) ) ))

(define (find-symbol id tree)
  (if (pair? tree)
      (or (find-symbol id (car tree))
          (find-symbol id (cdr tree)) )
      (eq? tree id) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Tests  (test-chap3a "src/chap3a.tst")

(define (test-chap3a file)
  (suite-test
   file
   "Scheme? "
   "Scheme= "
   #t
   (lambda (read check error)
     (set! wrong error)
     (lambda ()
       (check (eval (read))) ) )
   equal? ) )

;;; end of chap3a.scm
