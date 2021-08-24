;;; CODE TO SUPPORT CHAPTER 3 OF STRUCTURE AND INTERPRETATION OF
;;;  COMPUTER PROGRAMS
;;; NB. This code is *not* from the book

;;; In addition to code supplied here
;;;**For 3.4, might want parallel-execute as implemented for MIT Scheme
;;;**For 3.5, need stream special forms, which are not in Standard Scheme


;;For Section 3.1.2 -- written as suggested in footnote,
;; though the values of a, b, m may not be very "appropriately chosen"
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))


;;For Section 3.3.4, used by and-gate
;;Note: logical-and should test for valid signals, as logical-not does
(define (logical-and x y)
  (if (and (= x 1) (= y 1))
      1
      0))


;;For Section 3.5 -- useful for looking at finite amounts of infinite streams
;;Print the first n elements of the stream s.
;;One version prints on one line, one on separate lines

(define (print-n s n)
  (if (> n 0)
      (begin (display (stream-car s))
             (display ",")
             (print-n (stream-cdr s) (- n 1)))))

(define (print-n s n)
  (if (> n 0)
      (begin (newline)
	     (display (stream-car s))
             (print-n (stream-cdr s) (- n 1)))))


;;For Section 3.5.2, to check power series (exercises 3.59-3.62)
;;Evaluate and accumulate n terms of the series s at the given x
;;Uses horner-eval from ex 2.34
(define (eval-power-series s x n)
  (horner-eval x (first-n-of-series s n)))
(define (first-n-of-series s n)
  (if (= n 0)
      '()
      (cons (stream-car s) (first-n-of-series (stream-cdr s) (- n 1)))))
