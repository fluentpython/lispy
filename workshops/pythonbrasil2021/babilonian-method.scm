(define raiz2 (lambda (x)
    (raiz2-iter 1.0 x)))
    
(define raiz2-iter (lambda (chute x)
    (if (próximo? chute x)
        chute
        (raiz2-iter (melhorar chute x) x))))
        
(define próximo? (lambda (chute x)
    (< (abs (- (* chute chute) x)) 0.001)))
    
(define melhorar (lambda (chute x)
    (média chute (/ x chute))))
    
(define média (lambda (x y)
    (/ (+ x y) 2)))
    
(display (raiz2 12345654321))
