;;;; -*- scheme -*-
;;;; Copyright 2016 Demi Marie Obenour.
;;;;
;;;; Licensed under the Apache License, Version 2.0 or the MIT license at your
;;;; discretion.  This file may not be copied, modified, or distributed except
;;;; in accordence with those terms.

;;; Scheme environments are implemented as hash tables of linked lists.

(library
    (environment (0))
  (export
   lookup-environment
   expression-context?
   bind-variable
   bind-argument
   unbind-argument
   env?
   env.new
   env.table
   env.depth
   env.macros
   env.set-depth!)
  (import
   (rnrs)
   (bytecode)
   ;;(only (srfi :1) proper-list?)
   (srfi :9)
   (only (srfi :69) make-hash-table hash-table-ref hash-table-set!
         hash-table-update!))

  (define-record-type :env
    (env.raw-make table macros depth)
    env?
    (table env.table)
    (macros env.macros)
    (depth env.depth env.set-depth!))
  ;; A Scheme environment.
  ;;
  ;; A Scheme environment consists of key-value pairs.  The keys are symbols
  ;; and the values are association lists (of (depth, stack position) pairs).
  (define (env.new)
    (env.raw-make (make-hash-table) (make-hash-table)0))

  (define (expression-context? env) #f)
  (define (with-bindings env symbols exprs while-bound compile-expr bco)
    ;;(assert (proper-list? symbols))
    (assert (= (length symbols) (length exprs)))
    (let ((table (env.table env)))
      ;;(assert (hash-table? table))
      ;;(assert (proper-list? symbols))
      (let ((old-bindings
             (map (lambda (symbol)
                    (hash-table-ref table symbol (lambda () '())))
                  symbols)
             ))
        ;;(emit-bindings bco symbols exprs table compile-expr old-bindings)
        (while-bound)
        (map (lambda (symbol binding)
               (hash-table-set! table symbol binding))
             symbols old-bindings))))

  (define (bind-argument symbol env)
    (let ((nth (env.depth env)))
      (bind-symbol symbol env nth (cons nth 'argument))))

  (define (bind-variable symbol env)
    (let ((nth (env.depth env)))
      (bind-symbol symbol env nth nth)))

  (define (bind-symbol symbol env nth bindee)
    (let ((table (env.table env)))
      (hash-table-ref table symbol (lambda()'()))
      (hash-table-update!
       table symbol (lambda (plist) (cons bindee plist))
       (lambda () '()))
      (env.set-depth! env (+ 1 nth))))

  (define (unbind-argument symbol env)
    (assert (env? env))
    (hash-table-update!
     (env.table env) symbol
     (lambda (plist)
       (assert (and (pair? plist)
                    "Attempt to unbind a variable that was never bound!"
                    ))
       (cdr plist))
     (lambda ()
       (assert
        (and #f "Attempt to unbind a variable that was never bound!")))))

  (define (lookup-environment env symbol bco)
    (or (symbol? symbol)
        (error 'assert "cannot look up non-symbol" symbol))
    (let ((table (env.table env))
          (needs-update #f))
      (let ((old-binding
             (hash-table-ref table symbol
                             (lambda ()
                               (set! needs-update #t)
                               (list(emit-global bco symbol))))))
        (if needs-update
            (begin
              (hash-table-set! table symbol old-binding)
              old-binding)
            (car old-binding))))))
