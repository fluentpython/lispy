;;; compat.ss
;;; Robert Hieb & Kent Dybvig
;;; 92/06/18

;;; This file contains nonstandard help procedures.
;;; They are all present in Chez Scheme, but are easily defined
;;; in any standard Scheme system.
;;; These versions do no error checking.

; In Chez Scheme "(void)" returns an object that is ignored by the
; REP loop.  It is returned whenever a "nonspecified" value is specified
; by the standard.  The following should pick up an appropriate value.

(define void
   (let ((void-object (if #f #f)))
      (lambda () void-object)))

; "andmap" is like "map" except instead of "consing" the results it
; "ands" them, quitting early if #f" is obtained.
; The following does no error checking.

(define andmap
   (lambda (f first . rest)
      (cond
         ((null? rest)
          (let mapf ((l first))
             (or (null? l)
                 (and (f (car l)) (mapf (cdr l))))))
         ((null? (cdr rest))
          (let mapf ((l1 first) (l2 (car rest)))
             (or (null? l1)
                 (and (f (car l1) (car l2)) (mapf (cdr l1) (cdr l2))))))
         (else
          (let mapf ((first first) (rest rest))
             (or (null? first)
                 (and (apply f (car first) (map car rest))
                      (mapf (cdr first) (map cdr rest)))))))))
