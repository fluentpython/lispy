;;; init.ss
;;; Robert Hieb & Kent Dybvig
;;; 92/06/18

; These initializations are done here rather than "expand.ss" so that
; "expand.ss" can be loaded twice (for bootstrapping purposes).

(define expand-syntax #f)
(define syntax-dispatch #f)
(define generate-temporaries #f)
(define identifier? #f)
(define syntax-error #f)
(define syntax-object->datum #f)
(define bound-identifier=? #f)
(define free-identifier=? #f)
(define install-global-transformer #f)
(define implicit-identifier #f)
