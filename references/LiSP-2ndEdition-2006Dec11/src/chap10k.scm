;;; $Id: chap10k.scm,v 4.3 2006/11/25 17:44:26 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; CPS conversion

(define-class Continuation Function ())

(define-class Pseudo-Variable Local-Variable ())

;;; Redefine how forms are compiled.

(define (compile->C e out)
  (set! g.current '())
  (let* ((ee (letify (cpsify (Sexp->object e)) '()))
         (prg (extract-things! (lift! ee))) )
    (gather-temporaries! (closurize-main! prg))
    (generate-C-program out e prg) ) )

;;; Since Let forms are destructured during CPS, try to reintroduce
;;; them.  since CPS also duplicates parts of the AST, copy it
;;; completely to remove those sharing. Env is the Alist telling how
;;; to rename variables. The letify function now appears in chap10m.scm

;;; The entry point of the CPS transformation with the initial
;;; continuation: the identity!

(define (cpsify e)
  (let ((v (new-Variable)))
    (->CPS e (make-Continuation (list v) 
                                (make-Local-Reference v) )) ) )

(define new-Variable
  (let ((counter 0))
    (lambda ()
      (set! counter (+ 1 counter))
      (make-Pseudo-Variable counter #f #f) ) ) )

;;; Adapt the former compiler (chap10e) to these new classes.

(define-method (new-renamed-variable (variable Pseudo-Variable))
  variable )

(define-method (variable->C (variable Pseudo-Variable) out)
  (format out "v_~A" (Pseudo-Variable-name variable)) )

;;; For tests, use another library. CONS now has a ternary function as 
;;; value even if SCM_cons is still binary.

(set! *libraries* " scheme.o schemeklib.o ")

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define (convert2Regular-Application k . args)
  (make-Regular-Application k (convert2arguments args)) )

(define-generic (->CPS (e Program) k)
  (convert2Regular-Application k e) )

(define-method (->CPS (e Box-Write) k)
  (->CPS (Box-Write-form e)
         (let ((v (new-Variable)))
           (make-Continuation
            (list v) (convert2Regular-Application
                      k (make-Box-Write 
                         (Box-Write-reference e) 
                         (make-Local-Reference v) ) ) ) ) ) )

;;; The alternative duplicates continuations

(define-method (->CPS (e Alternative) k)
  (->CPS (Alternative-condition e)
         (let ((v (new-Variable)))
           (make-Continuation 
            (list v) (make-Alternative
                      (make-Local-Reference v)
                      (->CPS (Alternative-consequent e) k)
                      (->CPS (Alternative-alternant e) k) ) ) ) ) )

(define-method (->CPS (e Sequence) k)
  (->CPS (Sequence-first e)
         (let ((v (new-Variable)))
           (make-Continuation
            (list v) (->CPS (Sequence-last e) k) ) ) ) )

(define-method (->CPS (e Predefined-Application) k)
  (let* ((args (Predefined-Application-arguments e))
         (vars (let name ((args args))
                 (if (Arguments? args)
                     (cons (new-Variable) 
                           (name (Arguments-others args)) )
                     '() ) )) 
         (application 
          (convert2Regular-Application
           k
           (make-Predefined-Application
            (Predefined-Application-variable e)
            (convert2arguments
             (map make-Local-Reference vars) ) ) ) ) )
    (arguments->CPS args vars application) ) )

(define (arguments->CPS args vars appl)
  (if (pair? vars)
      (->CPS (Arguments-first args)
             (make-Continuation
              (list (car vars))
              (arguments->CPS (Arguments-others args) 
                              (cdr vars)
                              appl ) ) )
      appl ) )

(define-method (->CPS (e Regular-Application) k)
  (let* ((fun (Regular-Application-function e))
         (args (Regular-Application-arguments e))
         (varfun (new-Variable))
         (vars (let name ((args args))
                 (if (Arguments? args)
                     (cons (new-Variable) 
                           (name (Arguments-others args)) )
                     '() ) )) 
         (application 
          (make-Regular-Application
           (make-Local-Reference varfun)
           (make-Arguments 
            k (convert2arguments 
               (map make-Local-Reference vars) ) ) ) ) )
    (->CPS fun (make-Continuation
                (list varfun)
                (arguments->CPS args vars application) )) ) )

(define-method (->CPS (e Fix-Let) k)
  (->CPS (make-Regular-Application
          (make-Function (Fix-Let-variables e) (Fix-Let-body e))
          (Fix-Let-arguments e) )
         k ) )

(define-method (->CPS (e Global-Assignment) k)
  (->CPS (Global-Assignment-form e)
         (let ((v (new-Variable)))
           (make-Continuation 
            (list v) (convert2Regular-Application 
                      k (make-Global-Assignment
                         (Global-Assignment-variable e) 
                         (make-Local-Reference v) ) ) ) ) ) )

(define-method (->CPS (e Function) k)
  (convert2Regular-Application
   k (let ((k (new-Variable)))
       (make-Function (cons k (Function-variables e))
                      (->CPS (Function-body e) 
                             (make-Local-Reference k) ) ) ) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Tests 

(define (test-scheme10k file)
  (suite-test
   file
   "Scheme? "
   "Scheme= "
   #t
   (lambda (read check error)
     (set! objectify-error error)
     (set! evaluate-error error)
     (lambda ()
       (let ((e (read)))
         (cond ((member e *tests-to-skip*)
                (evaluate-error "Test to skip") )
               (else 
                (check (test-expression e)) ) ) ) ) )
   naive-match ) )

;;; end of chap10k.scm
