;
; a trivial implementation of property list routines
; get, put, remprop	oz/91
; 

(define *namelist* '())      ; top-level name/plst chain 
(define *lastlook* '(() ())) ; look-aside cache

; locate the name/plst pair

(define (nameprop name)
  (if (eq? name (car *lastlook*))
      *lastlook*
      (let ((pair (assq name *namelist*)))
        (if pair
            (set! *lastlook* pair))
        pair)))
;
; get returns property value found or '() to indicate absence
; of said property. As a consequence, '() cannot be a property value.
;
(define (get name prop)
  (let ((plst (nameprop name)))
    (if plst
        (let ((item (assq prop (cdr plst))))
          (if item
              (cdr item)
              '()))
        '())))

(define (put name prop valu)
  (let ((plst (nameprop name)))
    (if plst                              ; found a property list
	(let ((item (assq prop (cdr plst))))
          (if item
              (set-cdr! item valu)	  ; replace the current prop value
	      (begin
	       (set! item (cons prop valu))
	       (set-cdr! plst (cons item (cdr plst))))))
	(let ((item (cons prop valu)))
	  (set! plst (cons item '()))     ; create a new property list
	  (set! *namelist* (cons (cons name plst) *namelist*)))))
  valu)
  
(define (remq prop plst)
  (if (null? plst)
      '()
      (if (eq? prop (caar plst))
          (cdr plst)
          (let loop ((nlst plst))
            (if (null? (cdr nlst))
                plst
                (if (eq? prop (caadr nlst))
                    (begin (set-cdr! nlst (cddr nlst))
                           plst)
                    (loop (cdr nlst))))))))

(define (remprop name prop)
  (let ((plst (nameprop name)))
    (if plst
        (set-cdr! plst (remq prop (cdr plst)))
        '())))
