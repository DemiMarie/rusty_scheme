;;; -*- scheme -*-
(import
   (rnrs)
   (only (srfi :43) vector-copy)
   (tree-walk)
   (environment)
   (bytecode)
   (assembler)
   (ice-9 pretty-print))

(define (bound? sym) (symbol-bound? #f sym))
(define aset! vector-set!)
(define aref vector-ref)
(define (atom? obj) (not (pair? obj)))

(define (main args)
  (with-input-from-file (cadr args)
    (lambda ()
      (let ((list-to-build (cons #f '())))
        (let cont ((env (env.new))
                   (bco (create-bco))
                   (last-compiled-head list-to-build))
          (let ((res (read)))
            (if (eof-object? res)
                (cdr list-to-build)
                (let-values (((_ just-compiled)
                              (compile-form res env bco)))
                  (let ((instrs
                         (vector-copy (bco.instrs just-compiled)
                                      0
                                      (bco.len just-compiled)
                                      #f)))
                    (pretty-print
                     `#(,instrs
                        ,(vector-copy (bco.consts just-compiled)
                                      0
                                      (bco.consts-len just-compiled)
                                      #f)))
                    #;(pretty-print
                     (assemble-bytecode
                      (vector->list instrs)))
                      (cont env bco (list just-compiled)))))))))))

(pretty-print (main (command-line)))
