;;; $Id: chap8j.scm,v 1.1 1993/09/19 18:49:04 queinnec Exp $

;;; A reflective interpreter with non-systematically reified
;;; continuation and environment. These can be obtained through export
;;; and call/cc. Special forms are coded as fexprs.

;;; Bytecode size roughly 1K bytes.

(call/cc 
 (lambda (exit)
   ((lambda (extend! error make-toplevel)
      (begin 
                                        ; extend!
        (set! extend!
              (lambda (env names values)
                (if (pair? names)
                    (if (pair? values)
                        (begin (set-variable-value! (car names) (car values))
                               (extend! env (cdr names) (cdr values)) )
                        (error "Too few arguments" names) )
                    (if (symbol? names)
                        (set-variable-value! names values)
                        (if (null? names)
                            (if (null? values)
                                nil
                                (error "Too much arguments" values) )
                            nil ) ) ) ) )
                                        ; error
        (set! error
              (lambda (msg hint)
                (begin 
                  (display (list '*ERROR* msg hint))
                  (newline)
                  (exit 'aborted) ) ) )
                                        ; make-toplevel
        (set! make-toplevel
              (lambda (global-env prompt-in prompt-out)
                ((lambda (toplevel eval evlis eprogn reference 
                                   begin quote if set! lambda export )
                   (begin
                                        ; toplevel
                     (set! toplevel
                           (lambda () 
                             (begin (display prompt-in)
                                    ((lambda (result) 
                                       (begin (display prompt-out)
                                              (display result)
                                              (newline) ) )
                                     (eval (read) global-env) )
                                    (toplevel) ) ) )
                                        ; eval
                     (set! eval
                           (lambda (e r)
                             (if (pair? e) 
                                 ((lambda (f)
                                    (if (if (primitive? f)  ; or
                                            #t
                                            (continuation? f) )
                                        (apply f (evlis (cdr e) r))
                                        (apply f r (cdr e)) ) )
                                  (eval (car e) r) )
                                 (if (symbol? e) (reference r e) e) ) ) )
                                        ; evlis
                     (set! evlis
                           (lambda (e* r)
                             (if (pair? e*)
                                 ((lambda (v)
                                    (cons v (evlis (cdr e*) r)) )
                                  (eval (car e*) r) )
                                 nil ) ) )
                                        ; eprogn
                     (set! eprogn
                           (lambda (e+ r)
                             (if (pair? (cdr e+))
                                 (begin (eval (car e+) r)
                                        (eprogn (cdr e+) r) )
                                 (eval (car e+) r) ) ) )
                                        ; reference
                     (set! reference
                           (lambda (r name)
                             (if (variable-defined? name r)
                                 (variable-value name r)
                                 (if (variable-defined? name global-env)
                                     (variable-value name global-env)
                                     (error "No such variable" name) ) ) ) )
                                        ; begin
                     (set! begin
                           (lambda (r . forms)
                             (eprogn forms r) ) )
                                        ; quote
                     (set! quote
                           (lambda (r quotation) quotation) )
                                        ; if 
                     (set! if
                           (lambda (r condition then else)
                             (eval (if (eval condition r) then else) r) ) )
                                        ; set!
                     (set! set!
                           (lambda (r name form)
                             ((lambda (v) 
                                (if (variable-defined? name r)
                                    (set-variable-value! name r v)
                                    (if (variable-defined? name global-env)
                                        (set-variable-value! name global-env v)
                                        (error "No such variable" name) ) ))
                              (eval form r) ) ) )
                                        ; lambda
                     (set! lambda
                           (lambda (r variables body)
                             (lambda parameters
                               ((lambda (values)
                                  ((lambda (newenv)
                                     (begin (extend! newenv variables values)
                                            (eprogn newenv body) ) )
                                   (apply enrich r variables) ) )
                                (evlis r parameters) ) ) ) )
                                        ; export
                     (set! export
                           (lambda (r)
                             r ) )
                     (set! global-env (export))
                     (toplevel) ) )
                 'toplevel 'eval 'evlis 'eprogn 'reference 
                 'begin 'quote 'if 'set! 'lambda 'export ) ) )
                                        ; real work
        (make-toplevel (export) "?? " "== ") ) )
    'extend! 'error 'make-toplevel ) ) )

;;; end of chap8j.scm
