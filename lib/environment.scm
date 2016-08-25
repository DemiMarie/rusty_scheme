;;;; -*- scheme -*-
;;;; Copyright 2016 Demi Marie Obenour.
;;;;
;;;; Licensed under the Apache License, Version 2.0 or the MIT license at your
;;;; discretion.  This file may not be copied, modified, or distributed except
;;;; in accordence with those terms.

;;; Scheme environments are implemented as hash tables of linked lists.

#;(library
(environment (0))
(export
 lookup-environment
 expression-context?
 bind-variable
 bind-arguments
 unbind-argument
 env?
 env.new
 env.table
 env.depth
 env.macros
 env.set-depth!))
(import
 (rnrs)
 ;;(bytecode)
 ;;(only (srfi :1) proper-list?)
 (only (srfi :9) define-record-type)
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
                  (hash-table-ref table symbol (lambda ()
                                                 `((,symbol . global)))))
                symbols)
           ))
      ;;(emit-bindings bco symbols exprs table compile-expr old-bindings)
      (while-bound)
      (map (lambda (symbol binding)
             (hash-table-set! table symbol binding))
           symbols old-bindings))))

(define (bind-arguments symbols env)
  (let ((nth 0))
    (define (bind-arguments-internal symbols env)
      (if (not (null? symbols))
          (begin
            (bind-symbol (car symbols) env nth (cons nth 'argument))
            (set! nth (+ 1 nth))
            (bind-arguments-internal (cdr symbols) env))))
    (bind-arguments-internal symbols env)))

(define (bind-variable symbol env nth)
  (bind-symbol symbol env nth nth))

(define (bind-symbol symbol env nth bindee)
  (let ((table (env.table env)))
    (hash-table-ref table symbol (lambda()'()))
    (hash-table-update!
     table symbol (lambda (plist) (cons bindee plist))
     (lambda () `((,symbol . global))))
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
  (case symbol
    ;; Built-in functions
    ((apply
      ;; Vector ops
      vector-set! vector-length vector-ref make-vector vector?
      ;; List ops
      set-car! set-cdr! cons car cdr pair?
      ;; Math ops
      + - * / exp)
     (cons symbol 'primitive))
    (else
     (car
      (hash-table-ref (env.table env)
                      symbol
                      (lambda () (list (cons symbol 'global))))))))
