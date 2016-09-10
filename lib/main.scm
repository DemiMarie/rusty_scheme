;;; -*- scheme -*-
#;(import
   (rnrs)
   (only (srfi :43) vector-copy)
   (tree-walk)
   (environment)
   (bytecode)
   (assembler)
   (only (guile) parameterize)
   (ice-9 pretty-print))
(import (srfi :43))
(include "bytecode.scm")
(include "assembler.scm")
(include "environment.scm")
(include "tree-walk.scm")
(define (bound? sym) (symbol-bound? #f sym))
(define aset! vector-set!)
(define aref vector-ref)
(define (atom? obj) (not (pair? obj)))
(define (void) #t)
(define bco (create-bco))
(define env (env.new))
(define (compile-one-form)
  (let ((res (read)))
    (if (eof-object? res)
        (values)
        (begin
          (compile-toplevel-form res env bco)
          (compile-one-form)))))
(let ((tmp-bco (create-bco))
      (tmp-env (env.new)))
  (emit-constant tmp-bco 'alpha)
  (emit-constant tmp-bco 'alpha)
  (assert (= 1 (bco.consts-len tmp-bco))))
(define (compile-file filename)
  (with-input-from-file filename compile-one-form))
(define (main args)
  (for-each compile-file args)
  (assert (bco? bco))
  (assert (> (bco.len bco) 0))
  (let ((instrs
         (vector-copy (bco.instrs bco)
                      0
                      (bco.len bco)
                      #f)))
    
    #;(pretty-print
     (vector
      instrs
      (vector-copy (bco.consts bco)
                      0
                      (bco.consts-len bco)
                      #f)))
    (pretty-print (assemble-bytecode instrs))))
    #;(pretty-print
       (assemble-bytecode
       (vector->list instrs)))
(fluid-set! read-eval? #t)
(pretty-print (main '("system.lsp")))
