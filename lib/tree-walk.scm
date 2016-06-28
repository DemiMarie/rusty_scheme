;;;; Copyright 2016 Demi Marie Obenour.  Licensed under the Apache License,
;;;; Version 2.0 or the MIT license at your discretion.  This file may not be
;;;; copied, modified, or distributed except in accordence with those terms.

;;;; ### Tree walking and lowering – RustyScheme
;;;;
;;;; This library walks a Scheme program recursively,
;;;; compiling each form it reaches.  It makes several assumptions:
;;;;
;;;; - All `let` forms have been desugared.
;;;; - All special forms are valid.  This is checked, but the error messages
;;;;   are very poor.  It is much better for the checking to be done
;;;;   by the macro expander.

(library
    (tree-walk (1))
  (export compile-form)
  (import
   (rnrs)
   (bytecode)
   (srfi srfi-1)
   (srfi srfi-9))
  (begin
    (define (compile-let list env bco)
      (or (>= (length list) 2) (error "\"let\" takes at least 2 arguments"))
      (let ((bindings (car list)))
        (cond
         ((symbol? bindings) (error "Named \"let\" should have been desugared"))
         ((pair? bindings)
          (or (list? bindings) (error "\"let\" bindings must be proper list"))
          (map
           (lambda (binding)
             (or (= (length binding) 2) (error "invalid \"let\" binding"))
             (bind-variable
              env (car binding) (compile-form (cadr binding) env bco)))
           bindings))
         (else (error "Invalid \"let\" binding")))))

    (define (compile-lambda form env bco)
      (let ((list (car form)))
        (if (circular-list? list)
            (error "Circular list in lambda detected"))
        (let-values
            (((variadic? fixed-args)
              (let cont ((pair list)
                         (len 0))
                (cond
                 ((pair? pair)
                  (bind-variable env (car pair))
                  (cont (cdr pair) (+ 1 len)))
                 ((null? pair) (values #f len))
                 ((symbol? pair) (bind-variable env pair) (values #t len))
                 (else (error "Invalid lambda – non-symbol rest"))))))
          (emit-lambda-definition variadic? fixed-args
                                  (compile-sequence (cdr form))))))
    (define (compile-if pair env bco)
      (or (>= (length pair) 2) (error "\"if\" takes at least 2 arguments"))
      (emit-jump bco
                 (compile-form (car rest-of-form) env bco)
                 (compile-form (cadr rest-of-form) env bco)
                 (compile-sequence (cddr rest-of-form) env bco)))

    (define (compile-define rest-of-form env bco)
      (let ((defined (car rest-of-form)))
        (or (toplevel? env) (error "Non-toplevel define not yet implemented"))
        (if (pair? defined)
            (emit-set-global
             (car defined)
             (compile-lambda (cons (cdr defined) (cdr rest-of-form)) env bco))
            (emit-set-global defined (cdr rest-of-form)))))

    (define (compile-letrec list env bco)
      (or (>= (length list) 2) (error "\"letrec\" takes at least 2 arguments"))
      (let ((bindings (car list)))
        (cond
         ((symbol? bindings) (error "Named \"letrec\" not allowed"))
         ((pair? bindings)
          (let ((rest (cdr list)))
            (map
             (lambda (form) (compile-form form env bco))
             (map
              (lambda (binding)
                (or (= (length binding) 2) (error "invalid \"letrec\" binding"))
                (bind-variable env (car binding) #f)
                (cadr bindings))
              bindings)))
          (compile-sequence rest))
         (else (error "Invalid \"letrec\" form")))))

    (define (compile-pair pair env bco)
      (let ((rest-of-form (cdr pair)))
        (or (list? rest-of-form)
            (error "special form ~s must be a proper list"
                   (car pair)))
        (or (pair? (cdr rest-of-form))
            (error "special form ~a must be head of list of length at least 2"
                   (car pair)))
        (case (car pair)
          ((quote) (add-to-constant-vector bco rest-of-form))
          ((let) (compile-let rest-of-form env bco))
          ((if) (compile-if rest-of-form env bco))
          ((lambda) (compile-lambda rest-of-form env bco))
          ((define) (compile-define rest-of-form env bco))
          ((set!)  (compile-set! rest-of-form env bco))
          (else (compile-function-call rest-of-form env bco)))))


    (define (compile-function-call form env bco)
      (or (and (list? form)
               (pair? form))
          (error "Illegal function call"))
      (let ((function (lookup-environment env (car form))))
        ((if (symbol? function) ; builtin
             emit-primitive emit-apply) function
             (map (lambda (x) (compile-form x env bco)) (cdr form)))))

    (define (compile-set! form env bco)
      (or (and (list? form)
               (= (length form) 2))
          (error "Invalid set!"))
      (emit-set! (lookup-environment env (car form))
                 (compile-form (cddr form) env bco)))

    ;; Compile a given form to bytecode.
    ;;
    ;; Quote & self-evaluating forms are added to the constant vector.
    ;; Pairs are compiled to function calls and/or expanded specially.
    ;;
    ;; Return value: the stack index where the return value is located.
    (define (compile-form form env bco)
      (cond
       ((pair? form)
        (compile-pair form env bco))
       ((symbol? form)
        (compile-variable-reference form env bco))
       (else
        (emit-load bco (add-to-constant-vector bco form)))))))
