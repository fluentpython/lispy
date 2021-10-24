;;; $Id: chap10f.scm,v 4.0 1995/07/10 06:50:40 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Testing chap10e.scm: the compiler towards C.

;;; Compilation products that depend on the machine appear in o/${HOSTTYPE}
;;; Generated C appear directly in o/
;;; Sources of the C libraries are in src/c/

(define test-dir "o/")

(define *libraries* " scheme.o schemelib.o " )

(define *a.out* "chap10e")

(define Cfile (string-append *a.out* ".c"))

(define *cc+cflags* "gcc -ansi -pedantic -gg ")

(define (appear? s e)
  (or (eq? s e)
      (and (pair? e)
           (or (appear? s (car e))
               (appear? s (cdr e)) ) ) ) )

(define (test-scheme10e file)
  (suite-test
   file
   "Scheme? "
   "Scheme= "
   #t
   (lambda (read check error)
     (set! objectify-error error)
     (set! evaluate-error error)
     (lambda ()
       (let ((e (read)))
         (cond ((member e *tests-to-skip*)
                (evaluate-error "Test to skip") )
               ((appear? 'call/cc e)
                (check (test-expression 
                        `((lambda (call/cc) ,e) call/ep) )) )
               (else 
                (check (test-expression e)) ) ) ) ) )
   naive-match ) )

(define *tests-to-skip*
  '( (SET! XYZZY 3)
     ) )     

(define (test-expression e)
  ;;(set! objectify-error (lambda a (error 'test-expression "~A~%" a)))
  ;;(set! evaluate-error objectify-error)
  (call-with-output-file (string-append test-dir Cfile)
    (lambda (out)
      (compile->C e out) ) )
  (compile-and-run Cfile) )

(define (show-C-expression e)
  ;;(set! objectify-error (lambda a (error 'test-expression "~A~%" a)))
  ;;(set! evaluate-error objectify-error)
  (unveil (compile->C e (current-output-port))) )

;;; Pass the output of a.out through a sed to eliminate unreadable
;;; syntaxes such as #<closure...> etc.

(define (compile-and-run Cfile)
  (let* ((log (string-append *a.out* ".log"))
         (dir (string-append test-dir "${HOSTTYPE}"))
         (cmd (string-append 
               "cd " dir "; "
               *cc+cflags* " -I../../src/c "
               (string-append "../" Cfile)
               *libraries* "-o " *a.out* ))
         (status (system cmd)) )
    (unless (= status 0)
      (evaluate-error "C compilation aborted.") )
    (set! status 
          (system (string-append "cd " dir "; "
                                 *a.out* " > /tmp/ttlog " )) )
    (system (string-append 
             "sed -e 's:#<:\<:g' -e 's:@::g' < /tmp/ttlog >" log) )
    (unless (= status 0)
      (evaluate-error "C execution arborted" status) )
    (call-with-input-file log read) ) )

;;; (test-scheme10e "../scheme-tests.scm")

;;; This function may act as an application entry point with options
;;; being the list of options given on the Un*x command.  Compile a
;;; file to C and possibly to a stand-alone.
;;;    LiSP.book.cc file.scm [-o a.out] [-C file.c] 

(define (compiler-entry-point options)
  (let ((file #f)
        (ofile #f)
        (cfile #f)
        (verbose #f) )
    (let scan-options ((options options))
      (if (pair? options)
          (cond ((equal? (car options) "-o") 
                 (if (pair? (cdr options))
                     (begin (set! ofile (cadr options))
                            (scan-options (cddr options)) )
                     (compiler-option-error "Missing file.o" options) ))
                ((equal? (car options) "-C") 
                 (if (pair? (cdr options))
                     (begin (set! cfile (cadr options))
                            (scan-options (cddr options)) )
                     (compiler-option-error "Missing file.c" options) ))
                ((equal? (car options) "-v") 
                 (set! verbose #t)
                 (scan-options (cdr options)) )
                (else (if file 
                          (compiler-option-error "Too much file.scm" options)
                          (begin (set! file (car options))
                                 (scan-options (cdr options)) ) )) ) ) )
    (unless file (compiler-option-error "No file to compile"))
    (unless cfile (set! cfile *default-cfile*))
    (when verbose (display "Generate C code...")(newline))
    (let ((e `(begin . ,(file->list file))))
      (call-with-output-file cfile
        (lambda (out) (compile->C e out)) ) )
    (when verbose (display "Calling C compiler...")(newline))
    (let* ((cmd (string-append 
                 *cc+cflags* " -I" *h-dir* " " cfile
                 (if ofile (string-append " -o " ofile) "")
                 " -copt " *rtbook-library* ))
           (status (system cmd)) )
      (unless (= status 0)
        (compiler-option-error "C compilation failed!") )
      #t ) ) )

(define *default-cfile* "/tmp/tmp.c")
(define *h-dir* "src/c/scheme.h")
(define *rtbook-library* "o/HOSTTYPE/rtbook.a")

;;; Report errors

(define (compiler-option-error msg . culprits)
  (display "Error: " stderr-port)
  (display msg stderr-port)
  (map (lambda (c) (display c stderr-port)(display " " stderr-port))
       culprits )
  (newline stderr-port)
  (exit 1) )

;;; Read a whole file into a list of expressions.

(define (file->list filename)
  (call-with-input-file filename
    (lambda (in)
      (let loop ((e (read in)))
        (if (eof-object? e)
            '()
            (cons e (loop (read in))) ) ) ) ) )

;;; end of chap10f.scm
