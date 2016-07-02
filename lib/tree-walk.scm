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
    (tree-walk (0))
  (export compile-form)
  (import
   (rnrs)
   (srfi :1)
   (srfi :9)
   (environment))
  (define (check-let-bindings bindings bad-binding-msg
                              bad-all-bindings-msg)
    (or (proper-list? bindings)
        (error 'syntax #(bad-all-bindings-msg binding)))
    (for-each
     (lambda (binding)
       (or (and (proper-list? binding)
                (symbol? (car binding))
                (= (length binding) 2))
           (error 'syntax #(bad-binding-msg binding))))
     bindings))
  (define (source-location form) #f)
  #;(define (emit-lambda-definition immediately-invoked? form args env bco)
  (let ((locals (bind-variables args)))))

  (define (translate-define form)
    (let ((defined (car form)))
      ;; Transform ((a b) c) to (a . (lambda (b) c))
      (if (pair? defined)
          (cons
           (car defined)
           (compile-lambda
            (cons (cdr defined) (cdr form))))
          (cons defined (cdr form)))))
  ;; Compile a given form to bytecode.
  ;;
  ;; Quote & self-evaluating forms are added to the constant vector.
  ;; Pairs are compiled to function calls and/or expanded specially.
  ;;
  ;; Return value: the stack index where the return value is located.
  (define (compile-form form env bco)

    (define (compile-form form)
      (cond
       ((pair? form) ; Pair = function call OR special form
        (compile-pair form))
       ((symbol? form) ; Symbol = variable reference
        (compile-variable-reference (lookup-environment env form)))
       (else ; Anything else evaluates to itself
        (emit-load bco (add-to-constant-vector bco form)))))

    ;; Compile a `letrec` expression.  Instead, a macro is used.
    (define (compile-letrec list)
      (let ((bindings (car list)))
        (cond
         ((proper-list? bindings) ; The only valid case
          (check-let-bindings bindings
                              "invalid \"letrec\" binding"
                              "invalid \"letrec\" bindings")
          (compile-form
           (let ((rest (cdr list)))
             `((lambda ,(map cadr bindings)
                 ,@(map (lambda (bound) `(set! ,(car bound) ,(cadr bound)))
                        bindings)
                 ,@(if (and (pair? (car rest)) (eq? (caar rest) 'define))
                       `(((lambda () ,@rest)))
                       rest))
               ,@(map (lambda (_) #t) bindings)))))
         ((symbol? bindings) ; Attempt to write a named `letrec` form.
          ;; This is an easy error to make, so give a useful
          ;; error message.
          (error 'syntax (cons "Named \"letrec\" not allowed" bindings)))
         (else (error 'syntax (cons "Invalid \"letrec\" form" list))))))



    ;; Compile a `let` form.
    (define (compile-let list)
      (let ((bindings (car list)))
        (cond
         ((proper-list? bindings)
          (check-let-bindings bindings
                              "invalid \"let\" binding"
                              "invalid \"let\" bindings")
          (compile-form
           (cons
            (cons 'lambda (cons (map car bindings) (cdr list)))
            (map cadr bindings))))
         ((symbol? bindings)
          (error 'assert "Internal error: not yet implemented: named let"))
         (else (error 'syntax (cons "Invalid \"let\" form" list))))))

    ;; Compile a lambda
    (define (compile-lambda form)
      (let ((list (car form)))
        (if (circular-list? list)
            (error "Circular list in lambda detected"))
        (let-values
            (((variadic? fixed-args)
              (let cont ((pair list) (len 0))
                (cond
                 ((pair? pair)
                  (bind-variable env (car pair))
                  (cont (cdr pair) (+ 1 len)))
                 ((null? pair) (values #f len))
                 ((symbol? pair)
                  (bind-variable env pair)
                  (values #t len))
                 (else
                  (error 'syntax "\
Invalid lambda – non-symbol rest"))))))
          (emit-lambda-definition variadic? fixed-args
                                  (compile-sequence (cdr form))))))
    (define (compile-if pair)
      (or (>= (length pair) 2)
          (error "\"if\" takes at least 2 arguments"))
      (emit-jump bco
                 (lambda () (compile-form (car rest-of-form)))
                 (lambda () (compile-form (cadr rest-of-form)))
                 (lambda () (compile-sequence (cddr rest-of-form)))))

    (define (compile-define rest-of-form)
      (let ((defined (car rest-of-form)))
        (if (expression-context? env)
            (error 'syntax "declaration \"define\" not \
allowed in expression context"))
        (translate-define defined)))

    ;; Compile a pair (the only hard case)
    (define (compile-pair pair)
      (let ((rest-of-form (cdr pair))
            (head (car pair)))
        (if (pair? head)
            ;; Immediately applied simple lambdas are treated specially.
            ;; Specifically, they are treated as `let` forms.  This allows
            ;; `let` to desugar to `lambda` with no loss of performance.
            (if (and (eq? (car head) 'lambda)
                     (proper-list? head)
                     (> (length head) 2))
                (begin
                  ;; Immediately applied simple lambda
                  (or (= (length (cadr head)) (length rest-of-form))
                      (begin
                        (display (length (cadr head)))
                        (newline)
                        (display (length rest-of-form))
                        (newline)
                        (error 'syntax #("Wrong number of arguments \
to immediately-invoked lambda" pair))))
                  (with-bindings env (cadr head)
                                 (map compile-form rest-of-form)
                                 (lambda () (compile-sequence (cddr form)))))
                (compile-function-call (compile-pair head) rest-of-form))
            (case (car pair)
              ((quote) (add-to-constant-vector bco rest-of-form))
              ((let) (compile-let rest-of-form))
              ((letrec) (compile-letrec rest-of-form))
              ((begin) (compile-sequence rest-of-form))
              ((if) (compile-if rest-of-form))
              ((lambda) (compile-lambda rest-of-form))
              ((define) (compile-define rest-of-form))
              ((set!)  (compile-set! rest-of-form))
              ((()) (error
                     'syntax
                     "() is not legal Scheme – did you mean '()?"))
              (else (compile-function-call
                     (lookup-environment env) rest-of-form))))))

    (define (compile-sequence form)
      (or (proper-list? form)
          (error "Only proper lists allowed as sequences"))
      (and (null? form) (error "Sequences must be of positive length"))
      (let cont ((current-position form)
                 (internal-defines '()))
        (if (and (pair? current-position)
                 (eq? (car current-position) 'define))
            ;; internal `define` form
            (cont (cdr current-position)
                  (cons
                   (let ((rest (cdr current-position)))
                     (or (pair? rest)
                         (error 'syntax "Bad `define` form"))
                     (let ((defined (car rest)))
                       (if (pair? defined)
                           (list (car defined) 'lambda (cdr defined) (cdr rest))
                           rest)))
                   internal-defines))
            internal-defines))

      (map (lambda (form) (compile-form form)) form))

    (define (compile-function-call head args)
      (or (proper-list? form)
          (error 'syntax "Illegal function call"))
      (let ((function (lookup-environment env (car form))))
        ((if (symbol? function) ; builtin
             emit-primitive emit-apply) function
             (map (lambda (x) (compile-form x)) (cdr form)))))

    (define (compile-set! form)
      (or (and (list? form)
               (= (length form) 2))
          (error 'syntax "Invalid set!"))
      (emit-set! (lookup-environment env (car form))
                 (compile-form (cddr form))))
    (compile-form form)))
