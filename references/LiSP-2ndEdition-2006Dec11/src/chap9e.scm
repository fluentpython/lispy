;;; $Id: chap9e.scm,v 4.0 1995/07/10 06:52:21 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Convert Programs back to Scheme aspect

;;; R is the alist mapping variables to their new Scheme names.

(define-generic (->Scheme (e) r))

(define-method (->Scheme (e Reference) r)
  (variable->Scheme (Reference-variable e) r) )

(define-generic (variable->Scheme (e) r))

(define-method (variable->Scheme (e Variable) r)
  (Variable-name e) )

(define-method (variable->Scheme (e Local-Variable) r)
  (let ((v (assq e r)))
    (if (pair? v) (cdr v) 
        (->Scheme-error "Unexistant variable" e) ) ) )

(define (->Scheme-error msg e)
  (symbol-append '!!!!! (Variable-name e) '!!!!!) )

; This was only used when debugging
;(define-method (->Scheme (e Magic-Keyword) r)
;  (symbol-append '!?!?! (Magic-Keyword-name e) '!?!?!) )

(define-method (->Scheme (e Local-Assignment) r)
  `(set! ,(->Scheme (Local-Assignment-reference e) r)
         ,(->Scheme (Local-Assignment-form e) r) ) )

(define-method (->Scheme (e Global-Assignment) r)
  `(set! ,(variable->Scheme (Global-Assignment-variable e) r)
         ,(->Scheme (Global-Assignment-form e) r) ) )

(define-method (->Scheme (e Function) r)
  (define (renamings-extend r variables names)
    (if (pair? names)
        (renamings-extend (cons (cons (car variables) (car names)) r)
                          (cdr variables) (cdr names) )
        r ) )
  (define (pack variables names)
    (if (pair? variables)
        (if (Local-Variable-dotted? (car variables))
            (car names)
            (cons (car names) (pack (cdr variables) (cdr names))) )
        '() ) )
  (let* ((variables (Function-variables e))
         (new-names (map (lambda (v) (gensym))
                         variables ))
         (newr (renamings-extend r variables new-names)) )
    `(lambda ,(pack variables new-names) 
       ,(->Scheme (Function-body e) newr)) ) )

(define-method (->Scheme (e Alternative) r)
  `(if ,(->Scheme (Alternative-condition e) r)
       ,(->Scheme (Alternative-consequent e) r)
       ,(->Scheme (Alternative-alternant e) r) ) )

;;; suppress some begin forms to improve readability.

(define-method (->Scheme (e Sequence) r)
  (let ((first (->Scheme (Sequence-first e) r))
        (last  (->Scheme (Sequence-last e) r)) )
    (if (and (pair? last)
             (eq? (car last) 'begin) )
        `(begin ,first . ,(cdr last))
        `(begin ,first ,last) ) ) )

(define-method (->Scheme (e Constant) r)
  (let ((v (Constant-value e)))
    (if (or (number? v) (boolean? v))
        v `(quote ,v) ) ) )

(define-method (->Scheme (e Regular-Application) r)
  `(,(->Scheme (Regular-Application-function e) r)
    . ,(->Scheme (Regular-Application-arguments e) r) ) )

(define-method (->Scheme (e Arguments) r)
  (cons (->Scheme (Arguments-first e) r)
        (->Scheme (Arguments-others e) r) ) )

(define-method (->Scheme (e No-Argument) r)
  '() )

(define-method (->Scheme (e Predefined-Application) r)
  `(,(variable->Scheme (Predefined-Application-variable e) r)
    . ,(->Scheme (Predefined-Application-arguments e) r) ) )

(define-method (->Scheme (e Fix-Let) r)
  (->Scheme (make-Regular-Application
             (make-Function (Fix-Let-variables e)
                            (Fix-Let-body e) )
             (Fix-Let-arguments e) )
            r ) )

;;; end of chap9e.scm
