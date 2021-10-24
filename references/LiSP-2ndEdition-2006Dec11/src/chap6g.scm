;;; $Id: chap6g.scm,v 4.1 2006/11/27 11:23:31 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Explicitation of define (variant of chap6b.scm)
;;; Load before chap6a and chap6b

;;; The new variable is stored at the beginning of sg.current.

(define (adjoin-global-variable! name)
  (let ((index (g.current-extend! name)))
    (cdr (car g.current)) ) )

;;; Add (define ...) special form

(define (meaning e r)
  (if (atom? e)
      (if (symbol? e) (meaning-reference e r)
                      (meaning-quotation e r) )
      (case (car e)
        ((quote)  (meaning-quotation (cadr e) r))
        ((lambda) (meaning-abstraction (cadr e) (cddr e) r))
        ((if)     (meaning-alternative (cadr e) (caddr e) (cadddr e) r))
        ((begin)  (meaning-sequence (cdr e) r))
        ((set!)   (meaning-assignment (cadr e) (caddr e) r))
        ((define) (meaning-define (cadr e) (caddr e) r))
        (else     (meaning-application (car e) (cdr e) r)) ) ) )

;;; Must not check now if a global variable is initialized.

(define (meaning-reference n r)
  (let ((kind (compute-kind r n)))
    (if kind
        (case (car kind)
          ((local)
           (let ((i (cadr kind))
                 (j (cddr kind)) )
             (if (= i 0)
                 (lambda (sr k)
                   (k (activation-frame-argument sr j)) )
                 (lambda (sr k)
                   (k (deep-fetch sr i j)) ) ) ) )
          ((global)
           (let ((i (cdr kind)))
             (lambda (sr k)
               (let ((v (global-fetch i)))
                 (if (eq? v undefined-value)
                     (wrong "Uninitialized variable" n)
                     (k v)  ) ) ) ) )
          ((predefined)
           (let* ((i (cdr kind))
                  (value (predefined-fetch i)) )
             (lambda (sr k)
               (k value) ) ) ) )
        (static-wrong "No such variable" n) ) ) )

;;; This variable contains all the variables explicitely defined.

(define *defined* '())

(define (meaning-define n e r)
  (let ((b (memq n *defined*)))
    (if (pair? b) 
        (static-wrong "Already defined variable" n)
        (set! *defined* (cons n *defined*)) ) )
  (meaning-assignment n e r) )

;;; Compile a program into a stand-alone program. It will initialize
;;; the global modifiable environment before starting evaluation.

(define (stand-alone-producer e)
  (set! g.current (original.g.current))
  (let ((originally-defined (append (map car g.current)
                                    (map car g.init) )))
    (set! *defined* originally-defined)
    (let* ((m (meaning e r.init))
           (size (length g.current))
           (anormals (set-difference (map car g.current) 
                                     *defined* )) )
      (if (null? anormals)
          (lambda (sr k)
            (set! sg.current (make-vector size undefined-value))
            (m sr k) )
          (static-wrong "Not explicitely defined variables" 
                        anormals ) ) ) ) )

(define (set-difference set1 set2)
  (if (pair? set1)
      (if (memq (car set1) set2)
          (set-difference (cdr set1) set2)
          (cons (car set1) (set-difference (cdr set1) set2)) )
      '() ) )

;;; end of chap6g.scm
