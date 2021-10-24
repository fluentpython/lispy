;;; $Id: chap5-bench.scm,v 4.0 1995/07/10 06:51:22 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This expression serves to compare the speed of two denotational
;;; interpreters (chap5a and chap5d).

(begin
  (set! primes
        (lambda (n f max)
          ((lambda (filter)
             (begin
               (set! filter (lambda (p)
                              (lambda (n) (= 0 (remainder n p))) ))
               (if (> n max)
                   '()
                   (if (f n)
                       (primes (+ n 1) f max)
                       (cons n
                             ((lambda (ff)
                                (primes (+ n 1)
                                        (lambda (p) (if (f p) t (ff p)))
                                        max ) )
                              (filter n) ) ) ) ) ) )
           'wait ) ) )
  (display (primes 2 (lambda (x) f) 50)) )

;;; With interpreted-chap5a on blaye: 62. seconds
;;; With interpreted-chap5d on blaye: 58. seconds
;;; With compiled-chap5a on blaye: 20. seconds
;;; With compiled-chap5d on blaye: 12. seconds
;;; With interpreted-chap6a on blaye: 2. seconds
;;; Compiled to C on blaye: 0.02 seconds 
;;;               size: 
;;; text    data    bss     dec     hex
;;; 28672   4096    96      32864   8060

;;; end of chap5-bench.scm
