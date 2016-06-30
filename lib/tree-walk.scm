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
   (srfi :1)
   (srfi :9))
  (begin
    ;; Compile a given form to bytecode.
    ;;
    ;; Quote & self-evaluating forms are added to the constant vector.
    ;; Pairs are compiled to function calls and/or expanded specially.
    ;;
    ;; Return value: the stack index where the return value is located.
    (define compile-form
      (lambda (form env bco)
        (letrec
            ((compile-form
              (lambda (form)
                (cond
                 ((pair? form)
                  (compile-pair form))
                 ((symbol? form)
                  (compile-variable-reference form))
                 (else
                  (emit-load bco (add-to-constant-vector bco form))))))

             ;; Compile a `let` form.
             (compile-let
              (lambda (list)
                (or (>= (length list) 2)
                    (error 'syntax "\"let\" takes at least 2 arguments"))
                (let ((bindings (car list)))
                  (cond
                   ((symbol? bindings)
                    (error 'assert "internal compiler error: named \"let\" \
not desugared"))
                   ((pair? bindings)
                    (or (list? bindings)
                        (error 'syntax "\"let\" bindings must be proper list"))
                    (map
                     (lambda (binding)
                       (or (= (length binding) 2)
                           (error 'syntax "invalid \"let\" binding"))
                       (bind-variable
                        env (car binding) (compile-form (cadr binding))))
                     bindings))
                   (else (error 'syntax "Invalid \"let\" binding"))))))
             (compile-lambda
              (lambda (form)
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
                     (else (error "Invalid lambda – non-symbol rest"))))))
                    (emit-lambda-definition variadic? fixed-args
                                            (compile-sequence (cdr form)))))))
             (compile-if
              (lambda (pair)
                (or (>= (length pair) 2)
                    (error "\"if\" takes at least 2 arguments"))
                (emit-jump bco
                           (lambda () (compile-form (car rest-of-form)))
                           (lambda () (compile-form (cadr rest-of-form)))
                           (lambda () (compile-sequence (cddr rest-of-form))))))
             (compile-define
              (lambda (rest-of-form)
                (let ((defined (car rest-of-form)))
                  #;(error 'assert "internal compiler error: internal define \
                  not desugared by psyntax")
                  (if (expression-context? env)
                      (error 'syntax "declaration \"define\" not \
allowed in expression context")
                      ())
                  (if (pair? defined)
                      (emit-set-global
                       (car defined)
                       (compile-lambda (cons (cdr defined) (cdr rest-of-form))))
                      (emit-set-global defined (cdr rest-of-form))))))

             #;(define (compile-declarations forms)
               (or (proper-list? forms)
                   (compiler-syntax-error
                    forms
                    "invalid syntax: body of `let', \
`letrec', `let*', or `lambda' must be a proper list"))
               (or (pair? forms)
                   (compiler-syntax-error
                    forms
                    "invalid syntax: body of `let', \
`letrec', `let*', or `lambda' must have at least one form"))
               (let cont ((current-form (car forms))
                          (next-form (cdr form)))
                 (if (and (pair? forms) (eq? 'define (car forms)));`define' form
                     ()
                     #f)))
             (compile-letrec
              (lambda (list)
                (or (>= (length list) 2)
                    (error 'syntax "\"letrec\" takes at least 2 arguments"))
                (let ((bindings (car list)))
                  (cond
                   ((symbol? bindings)
                    (error 'syntax "Named \"letrec\" not allowed"))
                   ((pair? bindings)
                    (let ((rest (cdr list)))
                      (map compile-form
                           (map
                            (lambda (binding)
                              (or (= (length binding) 2)
                                  (error 'syntax "invalid \"letrec\" binding"))
                              (bind-variable env (car binding) #f)
                              (cadr bindings))
                            bindings)))
                    (compile-sequence rest))
                   (else (error 'syntax "Invalid \"letrec\" form"))))))

             (compile-pair
              (lambda (pair)
                (let ((rest-of-form (cdr pair)))
                  (or (list? rest-of-form)
                      (error 'syntax "special form ~s must be a proper list"
                             (car pair)))
                  (or (pair? (cdr rest-of-form))
                      (error 'syntax (format "special form ~a \
must be head of list of length at least 2" (car pair))))
                  (case (car pair)
                    ((quote) (add-to-constant-vector bco rest-of-form))
                    ((let) (compile-let rest-of-form))
                    ((letrec) (compile-letrec rest-of-form))
                    ((begin) (compile-sequence rest-of-form))
                    ((if) (compile-if rest-of-form))
                    ((lambda) (compile-lambda rest-of-form))
                    ((define) (compile-define rest-of-form))
                    ((set!)  (compile-set! rest-of-form))
                    (else (compile-function-call rest-of-form))))))
             (compile-sequence
              (lambda (form)
                (or (proper-list? form)
                    (error "Only proper lists allowed as sequences"))
                (if (null? form) (error "Sequences must be of positive length"))
                (map (lambda (form) (compile-form form)) form)))

             (compile-function-call
              (lambda (form)
                (or (proper-list? form)
                    (error 'syntax "Illegal function call"))
                (if (null? form)
                    (error 'syntax
                           "() is not legal Scheme – did you mean '()?"))
                (let ((function (lookup-environment env (car form))))
                  ((if (symbol? function) ; builtin
                       emit-primitive emit-apply) function
                       (map (lambda (x) (compile-form x)) (cdr form))))))
             (compile-set!
              (lambda (form)
                (or (and (list? form)
                         (= (length form) 2))
                    (throw 'syntax "Invalid set!"))
                (emit-set! (lookup-environment env (car form))
                           (compile-form (cddr form))))))
          (compile-form form))))))

(define 
