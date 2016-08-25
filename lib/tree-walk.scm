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

(import
 (only (rnrs base) car cdr pair? symbol? error define lambda)
 (rnrs base)
 (rnrs io simple)
 (only (rnrs eval) eval)
 (only (srfi :1) proper-list? circular-list? fold)
 (only (srfi :43) vector-copy)
 (only (srfi :69) hash-table-set! hash-table-ref)
 (only (guile) interaction-environment parameterize)
 (only (ice-9 pretty-print) pretty-print))

(define (translate-define form)
  "Convert (define (a b) q) to (set! a (lambda (b) q))"
  (assert (pair? form))
  #;(pretty-print form)
  (assert (or (eqv? (car form) 'define) (eqv? (car form) 'define-macro)))
  (if (pair? (cdr form))
      (let ((head (cadr form)))
        (if (pair? head)
            (list (car head) `(lambda ,(cdr head) ,@(cddr form)))
            (cdr form)))
      (error 'syntax "Bad define form" form)))

(define (check-let-bindings bindings bad-binding-msg
                            bad-all-bindings-msg)
  "Check that `let` bindings are valid"
  (or (proper-list? bindings)
      (error 'syntax (vector bad-all-bindings-msg bindings)))
  (for-each
   (lambda (binding)
     (or (and (pair? binding)
              (symbol? (car binding))
              (let ((rest (cdr binding)))
                (and (pair? rest)
                     (symbol? (car rest))
                     (null? (cdr rest)))))
         (error 'syntax bad-binding-msg binding bindings)))
   bindings))

(define (source-location form) #f)

;;; Contains code from system.lsp
(define (compile-letrec form env bco is-tail?)
  (compile-form
   (let ((binds (car form))
         (body (cdr form)))
     `((lambda ,(map car binds)
         ,@(map (lambda (b) `(set! ,@b)) binds)
         ,@body)
       ,@(map (lambda (x) (void)) binds)))
   env bco is-tail?))

;;; Contains code from system.lsp, which is not by me.
(define (compile-let form env bco is-tail?)
  (compile-form
   (let ((binds (car form))
         (body (cdr form)))
     (let ((lname #f))
       (if (symbol? binds)
           (begin (set! lname binds)
                  (set! binds (car body))
                  (set! body (cdr body))))
       (let ((thelambda
              `(lambda ,(map (lambda (c) (if (pair? c) (car c) c))
                             binds)
                 ,@body))
             (theargs
              (map (lambda (c) (if (pair? c) (cadr c) (void))) binds)))
         (cons (if lname
                   `(letrec ((,lname ,thelambda)) ,lname)
                   thelambda)
               theargs)))) env bco is-tail?))

;; Immediately applied simple lambdas are treated specially.
;; Specifically, they are treated as `let` forms.  This allows
;; `let` to desugar to `lambda` with no loss of performance.
(define (compile-initial-pair pair head rest-of-form env bco is-tail)
  (if (and (eq? (car head) 'lambda)
           (proper-list? head)
           (> (length head) 2))
      ;; Immediately applied simple lambda
      (let ((arglist (cadr head)))
        (if (not (= (length arglist) (length rest-of-form)))
            (error 'syntax "Wrong number of arguments \
to immediately-invoked lambda" pair))
        (let ((depth (stack-depth bco)))
          (for-each (lambda (x) (compile-form x env bco #f)) rest-of-form)
          (for-each (lambda (x)
                      (bind-variable x env (+ 1 depth))
                      (set! depth (+ 1 depth)))
                    arglist)
          (compile-form (cddr head) env bco is-tail)
          (for-each (lambda (sym) (unbind-argument sym env)) arglist)))
      (begin
        (compile-pair head env bco #f)
        (compile-function-call (stack-depth bco)
                               rest-of-form env bco is-tail)))
  (values))

;; Compile a pair (the only hard case)
(define (compile-pair pair env bco is-tail?)
  (assert (pair? pair))
  (let ((rest-of-form (cdr pair))
        (head (car pair)))
    (or (proper-list? rest-of-form)
        (error 'syntax "Pair to be compiled must be proper list" pair))
    (cond
     ((pair? head)
      (compile-initial-pair pair head rest-of-form env bco is-tail?))
     ((symbol? head)
      (case head
        ((quote)
         (if (and (pair? rest-of-form) (null? (cdr rest-of-form)))
             (emit-constant bco (car rest-of-form))
             (error 'syntax "Bad quote form" pair)))
        ((let) (compile-let rest-of-form env bco is-tail?))
        ((letrec) (compile-letrec rest-of-form env bco is-tail?))
        ((begin) (compile-sequence rest-of-form env bco is-tail?))
        ((if) (compile-if rest-of-form env bco is-tail?))
        ((lambda) (compile-lambda rest-of-form env bco))
        ((define) (compile-define pair env bco))
        ((set!) (compile-set! rest-of-form env bco))
        (else
         (let ((expander
                (hash-table-ref (env.macros env) head
                                (lambda () #f))))
           (assert (or expander (not (eq? head 'cond))))
           (if expander
               (begin
                 #;(pretty-print rest-of-form)
                 (compile-form (apply expander rest-of-form) env bco is-tail?))
               (begin
                 (compile-function-call
                  (lookup-environment env head bco) rest-of-form env bco
                  is-tail?)))))))
     (else
      (error 'syntax "Invalid procedure in function call" pair))))
  )

;; Compile a lambda
(define (compile-lambda form env bco)
  "Compile a lambda form to bytecode."
  ;;(assert (env? env))
  (let ((lambda-list (car form)))
    (if (circular-list? lambda-list)
        (error 'syntax "Circular list in lambda detected" lambda-list))
    (let-values
        (((variadic? fixed-args symbols)
          (let cont ((pair lambda-list) (len 0)
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
      (emit-lambda-definition bco variadic? fixed-args
                              (lambda ()
                                (bind-arguments symbols env)
                                (compile-sequence (cdr form) env bco #t)
                                (map (lambda (x) (unbind-argument x env))
                                     symbols)))))
  (values))

(define (compile-scope form env bco is-tail)
  "Compile a sequence to bytecode.  Internal defines are properly handled."
  (if (not (proper-list? form))
      (error 'syntax "Only proper lists allowed as sequences" form))
  (if (null? form)
      (error 'syntax "Sequences must be of positive length" form))
  (let ((set!-expressions '())
        (bound-vars '()))
    (let cont ((form form)
               (maybe-tail #t))
      (assert (pair? form))
      (let* ((head (car form))
             (rest (cdr form))
             (end-of-tree (and maybe-tail (null? rest))))
        (define (not-still-in-defines)
          (let ((set!-expressions (reverse! set!-expressions))
                (bound-vars (reverse! bound-vars)))
            (for-each (lambda (x)
                        (emit bco 'load-t)
                        (bind-variable env x (stack-depth bco))) bound-vars)
            (for-each (lambda (x) (compile-form x env bco #f)) set!-expressions)
            (compile-form head env bco is-tail)
            (if (pair? rest)
                (compile-sequence rest env bco (and is-tail end-of-tree)))
            (values)))
        (if (pair? head)
            (case (car head)
              ((begin)
               (if (cont (cdr head) end-of-tree)
                   ;; Yes – still scanning defines
                   (cont rest end-of-tree)
                   ;; No – switch to compiling a sequence
                   (begin
                     (compile-sequence rest env bco (and is-tail end-of-tree))
                     #f)))
              ((define)
               (if end-of-tree
                   ;; `define` form in tail position is an error,
                   ;; since the expression would not evaluate to any value
                   (error 'syntax "Sequence without expressions")
                   (let ((translated (translate-define (cdr head))))
                     (set! set!-expressions (cons translated set!-expressions))
                     (set! bound-vars (cons bound-vars (cadr translated)))
                     (cont rest end-of-tree))))
              (else (not-still-in-defines)))
            (not-still-in-defines))))))

(define (compile-if pair env bco is-tail)
  "Compile a Scheme `if` expression to Scheme bytecode"
  (let ((length-of-pair (length pair)))
    (or (and (>= length-of-pair 2) (<= length-of-pair 3))
        (error 'syntax "\"if\" takes at least 2 arguments, \
but not more than 3")))
  (emit-jump bco
             (lambda ()
               (compile-form (car pair) env bco #f))
             (lambda ()
               (compile-form (cadr pair) env bco is-tail))
             (lambda ()
               (let ((last-of-form (cddr pair)))
                 (compile-form
                  (if (null? last-of-form)
                      #t
                      (car last-of-form)) env bco is-tail)))))

(define (compile-define defined env bco)
  "Compile a toplevel `define` declaration"
  (if (expression-context? env)
      (error 'syntax "declaration \"define\" not \
allowed in expression context"))
  (let ((translated (translate-define defined)))
    (compile-form (cddr translated) env bco #t)
    (emit
     bco
     'toplevel-set!
     (car translated)))
  (values))

(define (compile-function-call function args env bco is-tail)
  "Compile an indirect function call"
  (if (circular-list? args)
      (error 'syntax "Illegal function call"))
  (if (and (pair? function)
           (eq? (cdr function) 'primitive))
      (begin
        (let ((params
               (map (lambda (arg)
                      (let ((val (and (symbol? arg)
                                      (lookup-environment env arg bco))))
                        (if (integer? val)
                            val
                            (begin
                              #;(begin
                              (display "compiling argument: ")
                              (write arg)
                              (newline))
                              (compile-form arg env bco #f)
                              (stack-depth bco)))))
                    args)))
          (apply emit bco (car function) params)))
      (begin
        (emit-load bco function)
        (for-each
         (lambda (x)
           (compile-form x env bco #f)) args)
        (emit bco (if is-tail 'tail-call 'call) (length args))))
  (values))

(define (compile-sequence form env bco maybe-tail)
  (if (null? form)
      ;; Scheme defines an empty sequence as an error
      (error 'syntax form "No expressions in sequence")
      (let cont ((current (car form))
                 (next (cdr form)))
        (let ((is-end (null? next)))
          (compile-form current env bco (and maybe-tail is-end))
          (if is-end
              (values)
              (begin
                (emit bco 'pop)
                (cont (car next) (cdr next)))))))
  (values))
(define (compile-set! form env bco)
  "Compile an assignment (`set!`)"
  (or (and (proper-list? form)
           (= (length form) 2))
      (error 'syntax "Invalid set!" form))
  (or (symbol? (car form)) (syntax-violation form))
  (compile-form (cadr form) env bco #f)
  (emit-set! bco
             (lookup-environment env (car form) bco))
  (values))

;; Compile a given form to bytecode.
;;
;; Quote & self-evaluating forms are added to the constant vector.
;; Pairs are compiled to function calls and/or expanded specially.
;;
;; Return value: the stack index where the return value is located.
(define (compile-form form env bco is-tail?)
  (cond
   ((pair? form) ; Pair = function call OR special form
    (begin (compile-pair form env bco is-tail?) #f))
   ((symbol? form) ; Symbol = variable reference
    (begin (emit-load bco (lookup-environment env form bco)) #f))
   ;; () unquoted is not legal Scheme, but Femtolisp's system.lsp (our stdlib)
   ;; depends on it being self-evaluating.
   ;;((eq? form '())
   ;; (error
   ;;  'syntax
   ;;  "() is not legal Scheme – did you mean '()?" form))
   (else ; Anything else evaluates to itself
    (begin (emit-constant bco form)) #f)))

;;; Compiles a given top-level form to bytecode.
;;;
;;; Acts like `compile-form`, except that `define`, `begin`, and `define-macro`
;;; are handled appropriately for the toplevel.
(define (compile-toplevel-form form env bco)
  (if (pair? form)
      (case (car form)
        ((define)
         (let ((translated (translate-define form)))
           (emit bco 'bind-variable (cadr translated))
           (compile-form translated env bco #f)))
        ((begin)
         (for-each (lambda (x)
                     (compile-toplevel-form x env bco))
                   (cdr form)))
        ((define-macro)
         (let ((form-to-execute
                (translate-define form)))
           (pretty-print (cadr form-to-execute))
           (hash-table-set! (env.macros env)
                            (car form-to-execute)
                            (eval
                             (cadr form-to-execute)
                             (interaction-environment)))))
        ((set-syntax!)
         (let ((rest-of-form (cdr form)))
           (or (pair? rest-of-form)
               (error 'syntax "bad set-syntax! – must be head of list of \
length 3" form))
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
                 (error 'syntax "Bad set-syntax! form" form)))))
        (else
         (compile-form form env bco #f)))
      (compile-form form env bco #f)))

(define (pp-compiled-form form)
  (let-values (((_ignored bco)
                (compile-form form (env.new)(create-bco) #t)))
    (pretty-print
     `#(,(vector-copy (bco.instrs bco) 0 (bco.len bco) #f)
        ,(vector-copy (bco.consts bco) 0 (bco.consts-len bco) #f)))))
