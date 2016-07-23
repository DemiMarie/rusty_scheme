;;; -*- scheme -*-
(import
   (rnrs)
   (only (srfi :43) vector-copy)
   (tree-walk)
   (environment)
   (bytecode)
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
                (let ((just-compiled
                       (cons
                        (let-values (((_ just-compiled)
                                      (compile-form res env bco)))
                          (pretty-print
                           `#(,(vector-copy (bco.instrs just-compiled)
                                            0
                                            (bco.len just-compiled)
                                            #f)
                              ,(vector-copy (bco.consts just-compiled)
                                            0
                                            (bco.consts-len just-compiled)
                                            #f)))
                          just-compiled)
                        '())))
                  (set-cdr! last-compiled-head just-compiled)
                  (set! last-compiled-head just-compiled)))))))))

(main (command-line))
