;;;; -*- scheme -*-
;;;; Copyright 2016 Demi Marie Obenour.
;;;;
;;;; Licensed under the Apache License, Version 2.0 or the MIT license at your
;;;; discretion.  This file may not be copied, modified, or distributed except
;;;; in accordence with those terms.

;;; ### Tree walking and lowering – RustyScheme
;;;
;;; This library walks a Scheme program recursively,
;;; compiling each form it reaches.  It makes several assumptions:
;;;
;;; - All `let` forms have been desugared.
;;; - All special forms are valid.  This is checked, but the error messages
;;;   are very poor.  It is much better for the checking to be done
;;;   by the macro expander.

(library
    (tree-walk)
  (export
   compile-form
   pp-compiled-form)
  (import
   (rnrs base)
   (rnrs io simple)
   (only (rnrs eval) eval)
   (only (srfi :1) proper-list? circular-list? fold)
   (only (srfi :43) vector-copy)
   (only (srfi :69) hash-table-set! hash-table-ref)
   (only (guile) interaction-environment)
   (environment)
   (bytecode)
   (only (ice-9 pretty-print) pretty-print))


  (define (translate-define form)
    "Convert (define (a b) q) to (define a (lambda (b) q))"
    (let ((head (car form)))
      (if (pair? head)
          `(,(car head) (lambda ,(cdr head) ,@(cdr form)))
          form)))

  (define (check-let-bindings bindings bad-binding-msg
                              bad-all-bindings-msg)
    "Check that `let` bindings are valid"
    (or (proper-list? bindings)
        (error 'syntax #(bad-all-bindings-msg binding)))
    (for-each
     (lambda (binding)
       (or (and (proper-list? binding)
                (symbol? (car binding))
                (= (length binding) 2))
           (error 'syntax bad-binding-msg binding bindings)))
     bindings))

  (define (source-location form) #f)

  ;; Compile a `letrec` expression.  Instead, a macro is used.
  (define (compile-letrec list env bco)
    (let ((bindings (car list)))
      (cond
       ((proper-list? bindings) ; The only valid case
        (check-let-bindings bindings
                            "invalid \"letrec\" binding"
                            "invalid \"letrec\" bindings")
        (let ((form-to-compile
               (let ((rest (cdr list)))
                 `((lambda ,(map car bindings)
                     ,@(map (lambda (bound) `(set! ,(car bound) ,(cadr bound)))
                            bindings)
                     ,@(if (and (pair? (car rest)) (eq? (caar rest) 'define))
                           `(((lambda () ,@rest)))
                           rest))
                   ,@(map (lambda (_) #t) bindings)))))
          (compile-form form-to-compile env bco)))
       ((symbol? bindings) ; Attempt to write a named `letrec` form.
        ;; This is an easy error to make, so give a useful
        ;; error message.
        (error 'syntax (cons "Named \"letrec\" not allowed" bindings)))
       (else
        (error 'syntax (cons "Invalid \"letrec\" form" list))))))

  ;; Compile a `let` form.
  (define (compile-let list env bco)
    (let ((bindings (car list)))
      (cond
       ((proper-list? bindings)
        (check-let-bindings bindings
                            "invalid \"let\" binding"
                            "invalid \"let\" bindings")
        (compile-form
         (cons
          (cons 'lambda (cons (map car bindings) (cdr list)))
          (map cadr bindings)) env bco 1))
       ((symbol? bindings)
        #;(error 'assert "Internal error: not yet implemented: named let")
        (compile-form
         (let ((real-bindings (cadr list)))
           (if (null? real-bindings)
               (error 'syntax "Missing bindings in named let" list))
           (check-let-bindings real-bindings
                               "invalid \"let\" binding"
                               "invalid \"let\" bindings")
           (write `(letrec ((,bindings
                             (lambda ,(map car real-bindings)
                               ,@(cddr list))))
                     (,bindings ,@(map cadr real-bindings))))
           (newline)
           `(letrec ((,bindings
                      (lambda ,(map car real-bindings)
                        ,@(cddr list))))
              (,bindings ,@(map cadr real-bindings)))) env bco))
       (else
        (error 'syntax (cons "Invalid \"let\" form" list))))))

  ;; Immediately applied simple lambdas are treated specially.
  ;; Specifically, they are treated as `let` forms.  This allows
  ;; `let` to desugar to `lambda` with no loss of performance.
  (define (compile-initial-pair pair head rest-of-form env bco)
    (if (and (eq? (car head) 'lambda)
             (proper-list? head)
             (> (length head) 2))
        ;; Immediately applied simple lambda
        (let ((arglist (cadr head)))
          (if (= (length arglist) (length rest-of-form))
              (let ((depth (stack-depth bco)))
                (for-each (lambda (form symbol)
                            (compile-form form env bco)
                            (bind-variable symbol env (+ 1 (stack-depth bco))))
                          rest-of-form arglist)
                (compile-sequence (cddr head) env bco)
                (for-each (lambda (sym) (unbind-argument sym env)) arglist)
                (emit-stack-reset bco (+ 1 depth))
                (values (+ 1 depth) bco))
              (error 'syntax "Wrong number of arguments \
to immediately-invoked lambda" pair)))
        (compile-function-call
         (compile-pair head env bco)
         rest-of-form env bco)))

  ;; Compile a pair (the only hard case)
  (define (compile-pair pair env bco)
    (assert (pair? pair))
    (let ((rest-of-form (cdr pair))
          (head (car pair)))
      (or (proper-list? rest-of-form)
          (error 'syntax "Pair to be compiled must be proper list" pair))
      (cond
       ((pair? head) (compile-initial-pair pair head rest-of-form env bco))
       ((symbol? head)
        (case head
          ((quote)
           (if (and (pair? rest-of-form) (null? (cdr rest-of-form)))
               (emit-constant bco (car rest-of-form))
               (error 'syntax "Bad quote form" pair)))
          ((let) (compile-let rest-of-form env bco))
          ((letrec) (compile-letrec rest-of-form env bco))
          ((begin) (compile-sequence rest-of-form env bco))
          ((if) (compile-if rest-of-form env bco))
          ((lambda) (compile-lambda rest-of-form env bco))
          ((define) (compile-define rest-of-form env bco))
          ((define-macro)
           (let ((form-to-execute
                  (translate-define rest-of-form)))
             (hash-table-set! (env.macros env)
                              (car form-to-execute)
                              (eval
                               (cadr form-to-execute)
                               (interaction-environment)))))
          ((set-syntax!)
           (or (pair? rest-of-form)
               (error 'syntax "bad set-syntax! – must be head of list of \
length 3" pair))
           (let ((name (car rest-of-form))
                 (bound (cdr rest-of-form)))
             (if (and (symbol? name)
                      (pair? bound)
                      (null? (cdr bound))
                      (pair? (car bound))
                      (eq? (caar bound) 'lambda))
               (hash-table-set! (env.macros env)
                                (car rest-of-form)
                                (eval (cadr (rest-of-form))
                                      (interaction-environment)))
               (error 'syntax "Bad set-syntax! form" pair))))
          ((set!) (compile-set! rest-of-form env bco))
          (else
           (let ((expander
                  (hash-table-ref (env.macros env) head
                                  (lambda () #f))))
             (assert (or expander (not (eq? head 'cond))))
             (if expander
                 (compile-form (apply expander rest-of-form) env bco)
                 (compile-function-call
                  (lookup-environment env head bco) rest-of-form env bco))))))
       (else
        (error 'syntax "Invalid procedure in function call" pair)))))

  ;; Compile a lambda
  (define (compile-lambda form env bco)
    "Compile a lambda form to bytecode."
    ;;(assert (env? env))
    (let ((list (car form)))
      (if (circular-list? list)
          (error 'syntax "Circular list in lambda detected" list))
      (let-values
          (((variadic? fixed-args symbols)
            (let cont ((pair list) (len 0)
                       (symbols '()))
              (cond
               ((pair? pair)
                (let ((symbol (car pair)))
                  (cont (cdr pair) (+ 1 len) (cons symbol symbols))))
               ((null? pair) (values #f len symbols))
               ((symbol? pair)
                (values #t len symbols))
               (else
                (error 'syntax "Invalid lambda – non-symbol rest" pair))))))
        (bind-arguments symbols env)
        (compile-sequence (cdr form) env bco)
        (map (lambda (x) (unbind-argument x env)) symbols))))

  (define (compile-internal-defines form-with-defines env bco)
    (define (translate-internal-define rest)
      (or (pair? rest)
          (error 'syntax "Bad `define` form" form-with-defines))
      (let ((defined (car rest)))
        (if (pair? defined)
            `(,(car defined)
              (lambda ,(cdr defined)
                ,@(cdr rest)))
            (list defined rest))))
    (let ((list-to-compile
           (fold
            (lambda (form rest)
              (if (and (pair? form)
                       (eq? (car form) 'define))
                  ;; internal `define` form
                  (if (null? (cdr rest))
                      (list
                       (cons (translate-internal-define (cdr form))
                             (car rest)))
                      (error 'syntax
                             "\"define\" form \
not allowed in expression position" form))
                  (cons form rest)))
            '(()) form-with-defines)))
      (if (null? (cdr list-to-compile))
          (error 'syntax "no expressions in sequence" form-with-defines
                 list-to-compile)
          (begin
            (newline)
            (newline)
            (pretty-print "\n")
            (pretty-print (reverse list-to-compile))
            #;(flush-output-port (standard-output-port))
            (compile-letrec (reverse list-to-compile) env bco)))))

  (define (compile-sequence form env bco)
    "Compile a sequence to bytecode.  Internal defines are properly handled."
    (or (proper-list? form)
        (error 'syntax "Only proper lists allowed as sequences" form))
    (and (null? form)
         (error 'syntax "Sequences must be of positive length" form))
    (let ((depth (stack-depth bco)))
      (if (and (pair? (car form))
               (eq? (caar form) 'define))
          (compile-internal-defines form env bco)
          (begin
            (for-each
             (lambda (form) (compile-form form env bco 1)) form)
            (emit-stack-adjust bco depth)))))
  (define (compile-if pair env bco)
    "Compile a Scheme `if` expression to Scheme bytecode"
    (let ((length-of-pair (length pair)))
      (or (and (>= length-of-pair 2) (<= length-of-pair 3))
          (error 'syntax "\"if\" takes at least 2 arguments, \
but not more than 3")))
    (emit-jump bco
               (lambda ()
                 (compile-form (car pair) env bco))
               (lambda ()
                 (compile-form (cadr pair) env bco))
               (lambda ()
                 (let ((last-of-form (cddr pair)))
                   (compile-form
                    (if (null? last-of-form)
                        #t
                        (car last-of-form)) env bco)))))

  (define (compile-define defined env bco)
    "Compile a toplevel `define` declaration"
    (if (expression-context? env)
        (error 'syntax "declaration \"define\" not \
allowed in expression context")
        #f)
    (let ((translated (translate-define defined)))
      (emit-toplevel-set!
       bco
       (car translated)
       (compile-form (cadr translated) env bco))))

  (define (compile-function-call function args env bco)
    "Compile an indirect function call"
    (if (circular-list? args)
        (error 'syntax "Illegal function call"))
    (let
        ((builtin?
          (and (pair? function)
               (eq? (cdr function) 'primitive))))
      (if (not builtin?)
          (begin
            (emit-load bco function)
            (for-each
             (lambda (x)
               (compile-form x env bco 1)) args)
            (emit-apply bco (length args)))
          (begin
            (let ((params
                   (map (lambda (arg)
                          (let ((val (and (symbol? arg)
                                          (lookup-environment env arg bco))))
                            (if (integer? val)
                                val
                                (begin
                                  (display "compiling argument: ")
                                  (write arg)
                                  (newline)
                                  (compile-form arg env bco)))))
                        args)))
              (apply emit bco (car function) params))))))
  (define (compile-set! form env bco)
    "Compile an assignment (`set!`)"
    (or (and (proper-list? form)
             (= (length form) 2))
        (error 'syntax "Invalid set!" form))
    (or (symbol? (car form)) (syntax-violation form))
    (compile-form (cadr form) env bco)
    (emit-set! bco
               (lookup-environment env (car form) bco)))

  ;; Compile a given form to bytecode.
  ;;
  ;; Quote & self-evaluating forms are added to the constant vector.
  ;; Pairs are compiled to function calls and/or expanded specially.
  ;;
  ;; Return value: the stack index where the return value is located.
  (define (compile-form form env bco . return-values)
    (assert (not (procedure? form)))
    (let ((main-retval
           (cond
            ((pair? form) ; Pair = function call OR special form
             (compile-pair form env bco))
            ((symbol? form) ; Symbol = variable reference
             (emit-load bco (lookup-environment env form bco)))
            #;((eq? form '())
             (error
              'syntax
              "() is not legal Scheme – did you mean '()?" form))
            (else ; Anything else evaluates to itself
             (emit-constant bco form)))))
      (values main-retval bco)))

  (define (pp-compiled-form form)
    (let-values (((_ignored bco)
                  (compile-form form (env.new)(create-bco))))
      (pretty-print
       `#(,(vector-copy (bco.instrs bco) 0 (bco.len bco) #f)
          ,(vector-copy (bco.consts bco) 0 (bco.consts-len bco) #f))))))
