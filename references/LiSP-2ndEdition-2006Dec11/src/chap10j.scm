;;; $Id: chap10j.scm,v 4.0 1995/07/10 06:50:44 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Simple-minded initialization analysis.
;;; Just use the regular code-walker that has the advantage to walk
;;; programs in the evaluation order.

;;; Retrofit the representation of global variables.

(define-class Global-Variable Variable (initialized?))

(define (objectify-free-global-reference name r)
  (let ((v (make-Global-Variable name #f)))
    (set! g.current (cons v g.current))
    (make-Global-Reference v) ) )

(define (compile->C e out)
  (set! g.current '())
  (let ((prg (extract-things! 
              (lift! (initialization-analyze! (Sexp->object e))) )))
    (gather-temporaries! (closurize-main! prg))
    (generate-C-program out e prg) ) )

;;; The analysis

(define (initialization-analyze! e)
  (call/cc (lambda (exit) (inian! e (lambda () (exit 'finished)))))
  e )

(define-generic (inian! (e) exit)
  (update-walk! inian! e exit) )

(define-method (inian! (e Global-Assignment) exit)
  (call-next-method)
  (let ((gv (Global-Assignment-variable e)))
    (set-Global-Variable-initialized?! gv #t)
    (inian-warning "Surely initialized variable" gv)
    e ) )

;;; Predefined global variables are initialized.

(define-method (inian! (e Global-Reference) exit)
  (let ((gv (Global-Reference-variable e)))
    (cond ((Predefined-Variable? gv) e)
          ((Global-Variable-initialized? gv) e)
          (else (inian-error "Surely uninitialized variable" gv)
                (exit) ) ) ) )

(define-method (inian! (e Alternative) exit)
  (inian! (Alternative-condition e) exit)
  (exit) )

(define-method (inian! (e Application) exit)
  (call-next-method)
  (exit) )

(define-method (inian! (e Function) exit)
  e )

;;; Modify the compiler.

(define-method (reference->C (v Global-Variable) out)
  (cond ((Global-Variable-initialized? v)
         (variable->C v out) )
        (else (format out "SCM_CheckedGlobal")
              (between-parentheses out
                (variable->C v out) ) ) ) )
        
;;; Trace

(define (inian-warning msg gv)
  (format #t "INIAN: ~A : ~A~%" (Global-Variable-name gv) msg) )

(define (inian-error msg gv)
  (format #t "INIAN: ~A : ~A~%" (Global-Variable-name gv) msg) )

;;; end of chap10j.scm
