;;; $Id: chap3g.scm,v 4.0 1995/07/10 06:51:16 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Addition of block, return-from features to chap3f.scm

(define (evaluate e r k)
  (if (atom? e)
    (cond ((symbol? e) (evaluate-variable e r k))
          (else        (evaluate-quote e r k)) )
    (case (car e)
     ((quote)  (evaluate-quote (cadr e) r k))
     ((if)     (evaluate-if (cadr e) (caddr e) (cadddr e) r k))
     ((begin)  (evaluate-begin (cdr e) r k))
     ((set!)   (evaluate-set! (cadr e) (caddr e) r k))
     ((lambda) (evaluate-lambda (cadr e) (cddr e) r k))
     ((block)       (evaluate-block (cadr e) (cddr e) r k))
     ((return-from) (evaluate-return-from (cadr e) (caddr e) r k))
     ((catch) (evaluate-catch (cadr e) (cddr e) r k))
     ((throw) (evaluate-throw (cadr e) (caddr e) r k))
     ((unwind-protect) (evaluate-unwind-protect (cadr e) (cddr e) r k))
     (else    (evaluate-application (car e) (cdr e) r k)) ) ) )

;;; BLOCK, RETURN-FROM

(define-class block-cont continuation (label))

(define-class block-env full-env (cont))

(define (evaluate-block label body r k)
  (let ((k (make-block-cont k label)))
    (evaluate-begin body
                    (make-block-env r label k)
                    k ) ) )

(define-method (resume (k block-cont) v)
  (resume (block-cont-k k) v) )

(define-generic (block-lookup (r) n k v)
  (wrong "not an environment" r n k v) )

(define-method (block-lookup (r block-env) n k v)
  (if (eq? n (block-env-name r))
      (unwind k v (block-env-cont r))
      (block-lookup (block-env-others r) n k v) ) )

(define-method (block-lookup (r full-env) n k v)
  (block-lookup (variable-env-others r) n k v) )

(define-method (block-lookup (r null-env) n k v)
  (wrong "Unknown block label" n r k v) )

(define-generic (unwind (k) v ktarget))

(define-method (unwind (k continuation) v ktarget)
  (if (eq? k ktarget) (resume k v)
      (unwind (continuation-k k) v ktarget) ) )

(define-method (unwind (k bottom-cont) v ktarget)
  (wrong "Obsolete continuation" v) )

(define-class return-from-cont continuation (r label))

(define (evaluate-return-from label form r k)
  (evaluate form r (make-return-from-cont k r label)) )

(define-method (resume (k return-from-cont) v)
  (block-lookup (return-from-cont-r k)
                (return-from-cont-label k)
                (return-from-cont-k k)
                v ) ) 

;;; CATCH, THROW

(define-class catch-cont continuation (body r))

(define-class labeled-cont continuation (tag))

(define (evaluate-catch tag body r k)
  (evaluate tag r (make-catch-cont k body r)) )

(define-method (resume (k catch-cont) v)
  (evaluate-begin (catch-cont-body k)
                  (catch-cont-r k)
                  (make-labeled-cont (catch-cont-k k) v) ) )

(define-method (resume (k labeled-cont) v)
  (resume (labeled-cont-k k) v) )

(define-class throw-cont continuation (form r))

(define-class throwing-cont continuation (tag cont))

(define (evaluate-throw tag form r k)
  (evaluate tag r (make-throw-cont k form r)) )

(define-method (resume (k throw-cont) tag)
  (catch-lookup k tag k) )

(define-generic (catch-lookup (k) tag kk)
  (wrong "Not a continuation" k tag kk) )

(define-method (catch-lookup (k continuation) tag kk)
  (catch-lookup (continuation-k k) tag kk) )

(define-method (catch-lookup (k bottom-cont) tag kk)
  (wrong "No associated catch" k tag kk) )

(define-method (catch-lookup (k labeled-cont) tag kk)
  (if (eqv? tag (labeled-cont-tag k)) ; comparator
      (evaluate (throw-cont-form kk)
                (throw-cont-r kk)
                (make-throwing-cont kk tag k) )
      (catch-lookup (labeled-cont-k k) tag kk) ) )

(define-method (resume (k throwing-cont) v)
  (resume (throwing-cont-cont k) v) )

;;; end of chap3g.scm
