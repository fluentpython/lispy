
(apply
 (lambda (make-toplevel make-flambda flambda? flambda-apply)
   (set! make-toplevel
         (lambda (prompt-in prompt-out)
           (call/cc 
            (lambda (exit)
              (monitor (lambda (c b) (exit b))
                ((lambda (it extend error global-env 
                          toplevel eval evlis eprogn reference )
                   (set! extend
                         (lambda (env names values)
                           (if (pair? names)
                               (if (pair? values)
                                   ((lambda (newenv)
                                      (begin 
                                        (set-variable-value! 
                                         (car names) newenv (car values) )
                                        (extend newenv (cdr names) 
                                                (cdr values) ) ) )
                                    (enrich env (car names)) )
                                   (error "Too few arguments" names) )
                               (if (symbol? names)
                                   ((lambda (newenv)
                                      (begin 
                                        (set-variable-value! 
                                         names newenv values )
                                        newenv ) )
                                    (enrich env names) )
                                   (if (null? names)
                                       (if (null? values)
                                           env
                                           (error "Too much arguments" 
                                                  values ) )
                                       env ) ) ) ) )
                   (set! error (lambda (msg hint)
                                 (exit (list msg hint)) ))
                   (set! toplevel
                         (lambda (genv) 
                           (set! global-env genv)
                           (display prompt-in)
                           ((lambda (result) 
                              (set! it result)
                              (display prompt-out)
                              (display result)
                              (newline) )
                            ((lambda (e)
                               (if (eof-object? e)
                                   (exit e)
                                   (eval e global-env) ) )
                               (read) ) )
                           (toplevel global-env) ) )
                   (set! eval
                         (lambda (e r)
                           (if (pair? e) 
                               ((lambda (f)
                                  (if (flambda? f)
                                      (flambda-apply f r (cdr e))
                                      (apply f (evlis (cdr e) r)) ) )
                                (eval (car e) r) )
                               (if (symbol? e) (reference e r) e) ) ) )
                   (set! evlis
                         (lambda (e* r)
                           (if (pair? e*)
                               ((lambda (v)
                                  (cons v (evlis (cdr e*) r)) )
                                (eval (car e*) r) )
                               '() ) ) )
                   (set! eprogn
                         (lambda (e+ r)
                           (if (pair? (cdr e+))
                               (begin (eval (car e+) r)
                                      (eprogn (cdr e+) r) )
                               (eval (car e+) r) ) ) )
                   (set! reference 
                         (lambda (name r)
                           (if (variable-defined? name r)
                               (variable-value name r)
                               (if (variable-defined? name global-env)
                                   (variable-value name global-env)
                                   (error "No such variable" name) ) ) ) )
                   ((lambda (quote if set! lambda flambda monitor)
                      (toplevel (the-environment)) )
                    (make-flambda 
                     (lambda (r quotation) quotation) )
                    (make-flambda
                     (lambda (r condition then else)
                       (eval (if (eval condition r) then else) r) ) )
                    (make-flambda
                     (lambda (r name form)
                       ((lambda (v) 
                          (if (variable-defined? name r)
                              (set-variable-value! name r v)
                              (if (variable-defined? name global-env)
                                  (set-variable-value! name global-env v)
                                  (error "No such variable" name) ) ))
                        (eval form r) ) ) )
                    (make-flambda
                     (lambda (r variables . body)
                       (lambda values
                         (eprogn body (extend r variables values)) ) ) )
                    (make-flambda
                     (lambda (r variables . body)
                       (make-flambda
                        (lambda (rr . parameters)
                          (eprogn body 
                                  (extend r variables 
                                          (cons rr parameters) ) ) ) ) ) )
                    (make-flambda
                     (lambda (r handler . body)
                       (monitor (eval handler r)
                                (eprogn body r) ) ) ) ) )
                 'it 'extend 'error 'global-env 
                 'toplevel 'eval 'evlis 'eprogn 'reference ) ) ) ) ) ) 
   (make-toplevel "?? " "== ") )
 'make-toplevel 
 ((lambda (flambda-tag)
    (list (lambda (behavior) (cons flambda-tag behavior))
          (lambda (o) (if (pair? o) (= (car o) flambda-tag) #f))
          (lambda (f r parms) (apply (cdr f) r parms)) ) )
  98127634 ) )

;;; end of chap8j.scm
