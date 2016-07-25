(letrec ((cont
          (lambda (env bco last-compiled-head)
            (let ((res (read)))
              (if (eof-object? res)
                  (cdr list-to-build)
                  (let ((just-compiled
                         (cons (compile-form res env bco) (quote ()))))
                    (set-cdr! last-compiled-head just-compiled)
                    (set! last-compiled-head just-compiled)))))))
  (cont (env.new) (create-bco) list-to-build))
