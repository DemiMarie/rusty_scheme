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
   (srfi :1)
   (srfi :69))
  ;; A Scheme environment
  (define make-environment make-hash-table)

  (define (expression-context? env) #t)
  ;; Bind `symbol` to a stack slot, run `func`, then reset the binding
  ;; of `symbol`.  Not exception safe!
  (define (with-binding env symbol binding func)
    (or (symbol? symbol)
        (error 'assert "internal error: tried to bind a non-symbol"))
    (let ((old-binding (hash-table-ref env symbol (lambda () #f))))
      (hash-table-set! env symbol binding)
      (func)
      (hash-table-set! env symbol old-binding)))
  (define (with-bindings env symbols bindings func)
    (or (= (length symbols) (length bindings)) (error 'assert "mismatch \
in lengths of symbols and bindings"))
    (let ((old-bindings
           (map (lambda (symbol)
                  (hash-table-ref env symbol (lambda () #f)))
                symbols)))
      (map (lambda (symbol binding) (hash-table-set! env symbol binding))
           symbols bindings)
      (func)
      (map (lambda (symbol binding) (hash-table-set! env symbol binding))
           symbols old-bindings)))
  (define (lookup-environment env symbol)
    (hash-table-ref env symbol #f)))
