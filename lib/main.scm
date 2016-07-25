;;; -*- scheme -*-
(import
   (rnrs)
   (only (srfi :43) vector-copy)
   (tree-walk)
   (environment)
   (bytecode)
   (assembler)
   (ice-9 pretty-print))


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
                    (pretty-print
                     (assemble-bytecode
                      (vector->list instrs)))
                    (let ((new-tail (list just-compiled)))
                      (set-cdr! last-compiled-head new-tail)
                      (cont env bco just-compiled)))))))))))

(pretty-print (main (command-line)))
