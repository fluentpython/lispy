;;; $Id: chap10l.scm,v 4.0 1995/07/10 06:50:49 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Testing chap10k.scm by direct interpretation of the objectified code.

(define (compile-expression e)
  (let* ((ee (letify (cpsify (Sexp->object e)) '()))
         (prg (extract-things! (lift! ee))) )
    ;;(gather-temporaries! (closurize-main! prg))
    prg ) )

;(define (objectify-free-global-reference variable r)
;  (objectify-error "No such variable" variable) )

(define-method (show (o Reference) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (format stream "#<Reference to ~A>"
            (Variable-name (Reference-variable o)) ) ) )

(define (symbolify . names)
  (string->symbol
   (apply string-append 
          (map (lambda (n)
                 (cond ((symbol? n) (symbol->string n))
                       ((string? n) n)
                       (else (format #f "~A" n)) ) )
               names ) ) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Convert an objectified program back to Scheme

(define-generic (->Sexp (e))
  e )

(define-method (->Sexp (e Reference))
  (variable->Sexp (Reference-variable e)) )

(define-method (->Sexp (e Free-Reference))
  `(*free* ,(variable->Sexp (Reference-variable e))) )

(define-method (->Sexp (e Global-Assignment))
  `(set! ,(variable->Sexp (Global-Assignment-variable e))
         ,(->Sexp (Global-Assignment-form e)) ) )

(define-generic (variable->Sexp (e)))

(define-method (variable->Sexp (e Variable))
  (Variable-name e) )

(define-method (variable->Sexp (e Pseudo-Variable))
  (symbolify 'v (Variable-name e)) )

(define-method (variable->Sexp (e Quotation-Variable))
  (symbolify 'q (Variable-name e)) )

(define-method (variable->Sexp (e Pseudo-Variable))
  (symbolify 'k (Variable-name e)) )

(define-method (->Sexp (e Box-Read))
  `(box-ref ,(->Sexp (Box-Read-reference e))) )

(define-method (->Sexp (e Box-Write))
  `(box-set! ,(->Sexp (Box-Write-reference e))
             ,(->Sexp (Box-Write-form e)) ) )

(define-method (->Sexp (e Box-Creation))
  `(set! ,(variable->Sexp (Box-Creation-variable e))
         (make-box ,(variable->Sexp (Box-Creation-variable e))) ) )

(define-method (->Sexp (e Function))
  `(lambda ,(map variable->Sexp (Function-variables e))
     ,(->Sexp (Function-body e)) ) )

(define-method (->Sexp (e Flat-Function))
  `(lambda ,(map variable->Sexp (Function-variables e))
     (*free-variables* . ,(->Sexp (Flat-Function-free e)))
     ,(->Sexp (Function-body e)) ) )

(define-method (->Sexp (e Alternative))
  `(if ,(->Sexp (Alternative-condition e))
       ,(->Sexp (Alternative-consequent e))
       ,(->Sexp (Alternative-alternant e)) ) )

(define-method (->Sexp (e Sequence))
  `(begin ,(->Sexp (Sequence-first e))
          ,(->Sexp (Sequence-last e)) ) )

(define-method (->Sexp (e Constant))
  `(quote ,(Constant-value e)) )

(define-method (->Sexp (e Regular-Application))
  `(,(->Sexp (Regular-Application-function e))
    . ,(->Sexp (Regular-Application-arguments e)) ) )

(define-method (->Sexp (e No-Argument))
  '() )

(define-method (->Sexp (e Arguments))
  (cons (->Sexp (Arguments-first e))
        (->Sexp (Arguments-others e)) ) )

(define-method (->Sexp (e Predefined-Application))
  `(,(variable->Sexp (Predefined-Application-variable e))
    . ,(->Sexp (Predefined-Application-arguments e)) ) )

(define-method (->Sexp (e Fix-Let))
  `(let ,(map (lambda (var val) `(,var ,val))
              (map variable->Sexp (Fix-Let-variables e))
              (->Sexp (Fix-Let-arguments e)) )
     ,(->Sexp (Fix-Let-body e)) ) )

(define-method (->Sexp (e Free-Environment))
  (cons (->Sexp (Free-Environment-first e))
        (->Sexp (Free-Environment-others e)) ) )

(define-method (->Sexp (e No-Free))
  '() )

(define-method (->Sexp (e Flattened-Program))
  `(let ,(map (lambda (qv) `(,(variable->Sexp qv)
                             ',(Quotation-Variable-value qv) ) )
              (reverse (Flattened-Program-quotations e)) )
     (let ,(map (lambda (def) 
                  `(,(symbolify 'function (Function-Definition-index def))
                    ,(->Sexp def) ) )
                (Flattened-Program-definitions e) )
       ,(->Sexp (Flattened-Program-form e)) ) ) )

(define-method (->Sexp (e Closure-Creation))
  `(make-closure ',(symbolify 'function (Closure-Creation-index e))
                 . ,(->Sexp (Closure-Creation-free e)) ) )

;;; end of chap10l.scm
