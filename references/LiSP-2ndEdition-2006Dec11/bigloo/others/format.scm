;
; FILE:		"format.scm"
; IMPLEMENTS:	Format function {Scheme} -- see documentation below.
; AUTHOR:	Ken Dickey
; DATE:		1988
; LAST UPDATED:	1991 April 4
; NOTES:	Imports PRETTY-PRINT (~g) and OBJECT->STRING, et
;		cetera, from Mark Feeley's pretty printer code.
;
;		Does *not* implement ~| option.
;		The ~& option is bogus: it always prints a newline.

;;; [QNC] Changed error into format-error.
(define format-error 'wait)
(set! format-error tester-error)
;;; [QNC] unified case to lower-case everywhere.

;;  
;;  
;;  ========
;;  FUNCTION: (FORMAT <port> <format-string> . <args>)
;;  ========
;;  
;;  RESULT: returns unconsumed <args> or a string; has side effect of
;;  printing according to <format-string>.  If <port> is #t the output is
;;  to the current input port.  If <port> is #f, a formatted string is
;;  returned as the result of the call.  Otherwise <port> must be an
;;  output port.  <format-string> must be a string.  Characters are output
;;  as if the string were output by the DISPLAY function with the
;;  exception of those prefixed by a tilde (~) as follows [note that options
;;  which take arguments remove them from the argument list (they are said to
;;  be `consumed')]:
;;
;;option  mnemonic: description
;;------  ------------------------
;;    ~a  any: display the argument (as for humans).
;;    ~s  slashified: write the argument (as for parsers).
;;    ~d  decimal: the integer argument is output in decimal format.
;;    ~x  hexadecimal: the integer argument is output in hexadecimal format.
;;    ~o  octal: the integer argument is output in octal format.
;;    ~b  binary: the integer argument is output in binary format.
;;    ~p  plural: if the argument is greater than 1, a lower case 's' is printed.
;;    ~c  character: the next argument is displayed as a character.
;;    ~_  space: output a space character.
;;    ~%  newline: output a newline character.
;;    ~&  freshline: unless at the beginning of a line, same as ~%, else ignored.
;;    ~~  tilde: output a tilde.
;;    ~t  tab: output a tab charcter. **implemented, but system dependent**
;;    ~g  glorify: pretty print the argument (typically an s-expression).
;;    ~?  indirection: take the next argument as a format string and consume
;;        further arguments as appropriate, then continue to process the current
;;        format string.
;;
;; NYI 8^(
;;    ~|  page seperator: output a page seperator.
;;  

;----- IMPLEMENTATION SPECIFIC OPTIMIZATIONS

;;e.g.:
;; (##declare (fixnum)) ;; GAMBIT (v1.4)

(define tab-character (integer->char 9))   ;; NB: assumes ASCII encoding!!!
(define no-value (string->symbol ""))  ;; the sometimes-non-printing object

;----- requires Marc Feeley's PP

(define (object->display-string obj)
  (let ( (result '()) )
    (generic-write obj #t #f (lambda (str) (set! result (cons str result)) #t))
    (reverse-string-append result)))

;---------- FORMAT


(define (format <output-port> <format-string> . <args>)

  (letrec (
	   (port (if (eq? <output-port> #t)
		     (current-output-port)
		     <output-port>))
	   (char-write	   (lambda (any) (write-char any port)))
	   (string-display (lambda (any) (display any port)))
	   (do-write	   (lambda (any) (write   any port)))
	   (do-display	   (lambda (any) (display any port)))
	   (return-value   (lambda () no-value)) ; default
          )

    (define (format-help format-strg arglyst)

      (letrec
	  (
	   (length-of-format-string (string-length format-strg))

	   (anychar-dispatch
	    (lambda (pos arglist)
	      (if (>= pos length-of-format-string)
		  arglist ; used for ~? continuance
		  (let ( (char (string-ref format-strg pos)) )
		    (cond
		     ((eqv? char #\~)		;; eqv? was eq?
		      (tilde-dispatch (+ pos 1) arglist))
		     (else
		      (char-write char)
		      (anychar-dispatch (+ pos 1) arglist)
		      ))
		    ))
	      )) ; end anychar-dispatch

	    (tilde-dispatch
	     (lambda (pos arglist)
	       (cond
		((>= pos length-of-format-string)
		 (char-write #\~) ; tilde at end of string is just output
		 arglist ; used for ~? continuance
		 )
		(else
		 (case (char-upcase (string-ref format-strg pos))
		   ((#\A)	; Any -- for humans
		    (do-display (car arglist))
		    (anychar-dispatch (+ pos 1) (cdr arglist))
		    )
		   ((#\S)	; Slashified -- for parsers
		    (do-write (car arglist))
		    (anychar-dispatch (+ pos 1) (cdr arglist))
		    )
		   ((#\D)	; Decimal
		    (string-display (number->string (car arglist) 10))
		    (anychar-dispatch (+ pos 1) (cdr arglist))
		    )
		   ((#\X)	; Hexadecimal
		    (string-display (number->string (car arglist) 16))
		    (anychar-dispatch (+ pos 1) (cdr arglist))
		    )
		   ((#\O)	; Octal
		    (string-display (number->string (car arglist)  8))
		    (anychar-dispatch (+ pos 1) (cdr arglist))
		    )
		   ((#\B)	; Binary
		    (string-display (number->string (car arglist)  2))
		    (anychar-dispatch (+ pos 1) (cdr arglist))
		    )
		   ((#\C)	; Character
		    (char-write (car arglist)) 
		    (anychar-dispatch (+ pos 1) (cdr arglist))
		    )
		   ((#\P)	; Plural
		    (if (= (car arglist) 1)
			#f ; no action
			(char-write #\s))
		    (anychar-dispatch (+ pos 1) (cdr arglist))
		    )
		   ((#\~)	; Tilde
		    (char-write #\~)
		    (anychar-dispatch (+ pos 1) arglist)
		    )
		   ((#\%)	; Newline
		    (char-write #\newline)
		    (anychar-dispatch (+ pos 1) arglist)
		    )
		   ((#\_)	; Space
		    (char-write #\space)
		    (anychar-dispatch (+ pos 1) arglist)
		    )
		   ((#\&)	; Freshline -- Not Yet Implemented
		    (char-write #\newline)
		    (anychar-dispatch (+ pos 1) arglist)
		    )
		   ((#\T)	; Tab -- Implementation dependent
		    (char-write tab-character) 
		    (anychar-dispatch (+ pos 1) arglist)
		    )
;;                 ((#\|)	; Page Seperator -- Implementation dependent
;;                     (char-write #\???) 
;;                     (anychar-dispatch (+ pos 1) arglist)
;;                  )
		   ((#\G)	; Pretty-print {T}
		    (if (eq? port #f)
			(string-display (pretty-print-to-string (car arglist)))
			(pretty-print (car arglist) port))
		    (anychar-dispatch (+ pos 1) (cdr arglist))
		    )
		   ;; {"~?" in Common Lisp is "~K" in T}
                   ;; [QNC] Removed
		   ;;((#\?)	; indirection -- take next arg as format string.
		   ;; (anychar-dispatch (+ pos 1) 
		   ;;	      (format-help (car arglist) (cdr arglist)))
		   ;;                   ; Note: format-help returns unused args
		   ;; )
		   (else
                    (format-error "FORMAT: unknown tilde escape" 
                           (string-ref format-strg pos)))
		   )))
	       )) ; end tilde-dispatch

	    )			 

	; format-help main 
	(anychar-dispatch 0 arglyst)
	)) ; end format-help

    ; format main

    (if (eq? <output-port> #f)  ;; format to a string
	(let (
	      (out-string-list '())
	      (out-char-list   '())
	     )
	  ;; Grody code!!!  Flush this if we ever get standard string-ports!!
	  (set! do-write
		(lambda (any) (string-display (object->string any))) )
	  (set! do-display
		(lambda (any) (string-display (object->display-string any))))
	  (set! char-write  
		(lambda (c) (set! out-char-list (cons c out-char-list))) )
	  (set! string-display
		(lambda (str)
		  (cond
		   ((null? out-char-list)
		    (set! out-string-list (cons str out-string-list))
		   )
		   (else ;; make chars into string 
		    (set! out-string-list
			  (cons str
				(cons (list->string (reverse out-char-list))
				      out-string-list))
			  )
		    (set! out-char-list '())
		    )))
	   )
	  (set! return-value
		(lambda ()
		  (apply string-append
			 (reverse (if (null? out-char-list)
				      out-string-list
				      (cons (list->string
					     (reverse out-char-list))
					    out-string-list))))))
    )) ;; end-if outport #f

    (format-help <format-string> <args>)
    (return-value)
    

    ) ; end let
) ; end format
