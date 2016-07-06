;;;; -*- scheme -*-
;;;; Copyright 2016 Demi Marie Obenour.
;;;;
;;;; Licensed under the Apache License, Version 2.0 or the MIT license at your
;;;; discretion.  This file may not be copied, modified, or distributed except
;;;; in accordence with those terms.

;;; Scheme environments are implemented as hash tables of linked lists.

(library
    (environment (0))
  (export make-environment
          lookup-environment
          bind-variable
          bind-variables
          with-binding
          with-bindings
          expression-context?)
  (import
   (rnrs)
   (bytecode)
   (srfi :1)
   (srfi :69))

  ;; A Scheme environment
  (define make-environment make-hash-table)

  (define (expression-context? env) #f)
  (define (with-bindings env symbols exprs while-bound compile-expr bco)
    (assert (proper-list? symbols))
    (assert (= (length symbols) (length exprs)))
    (let ((old-bindings
           (map (lambda (symbol)
                  (hash-table-ref env symbol (lambda () #f)))
                symbols)))
      (emit-bindings bco symbols exprs env compile-expr)
      (while-bound)
      (map (lambda (symbol binding) (hash-table-set! env symbol binding))
           symbols old-bindings)))
  (define (lookup-environment env symbol bco)
    (or (symbol? symbol) (error 'assert "cannot look up non-symbol" symbol))
    (hash-table-ref env symbol (lambda() (emit-global bco symbol)))))
