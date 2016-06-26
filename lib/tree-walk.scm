(library
    (bytecomp tree-walk)
  (import
   (scheme base)
   (srfi-1)
   (sfri-9))
  (export compile-form)
  (begin
    (define (compile-let form env)
      (let (       )))

    (define (compile-if pair env bco)
      (or (>= (length pair) 2) (error "\"if\" takes at least two arguments"))
      (emit-jump bco
                 (compile-form (car rest-of-form) env)
                 (compile-form (cadr rest-of-form) env)
                 (compile-sequence (cddr rest-of-form) env)))

    (define (compile-define rest-of-form env bco)
      (let ((defined (car rest-of-form)))
        (or (toplevel? env) (error "Non-toplevel define not yet implemented"))
        (if (pair? defined)
            (emit-set-global
             (car defined)
             (compile-lambda (cons (cdr defined) (cdr rest-of-form))))
            (emit-set-global defined (cdr rest-of-form)))))
    (define (compile-pair pair env bco)
      (let ((rest-of-form (cdr pair)))
        (or (list? rest-of-form) (error "\"if\" form must be a proper list"))
        (case (car pair)
          ((quote) (add-to-constant-vector bco rest-of-form))
          ((let) (compile-let rest-of-form env bco))
          ((if) (compile-if rest-of-form env bco))
          ((lambda) (compile-lambda rest-of-form env bco))
          ((define) (compile-define rest-of-form env bco))
          (else compile-function-call rest-of-form env bco))))

    ;; Compile a given form to bytecode.
    ;;
    ;; Quote & self-evaluating forms are added to the constant vector.
    ;; Pairs are compiled to function calls and/or expanded specially.
    ;;
    ;; Return value: the stack index where the return value is located.
    (define (compile-form bco form)
      (cond
       ((pair? form)
        (compile-pair bco form))
       ((symbol? form)
        (compile-variable-reference bco form))
       (else
        (emit-load bco (add-to-constant-vector bco form)))))

    (define (Bytecode.new)
      (make-bytecode-object (make-vector #x10) 0 (make-vector #x10) 0))

    (define (add-bytecode bco bytecodes)
      (let ((bco-vector (get-bytecode bco))
            (bco-length (get-bytecode-length bco)))
        (let ((capacity (get-bytecode-length bco-vector)))
          ())))

    (define (compile-function-call environment port head rest args)
      (case (car form)
        ((quote)
         (write-char port load-opcode)
         (write-char port )
         ((eq?)
          (write-char port pop-opcode)))))


    (define (byte-compile form args)
      (let ((environment (make-hash-table)))
        (call-with-output-file filename
          (lambda (port)
            (cond ((pair? form)
                   (compile-function-call environment port
                                          (car form) (cdr form) args))
                  ((symbol? form)
                   (combile-variable-reference environment port))
                  (#t (compile-literal form args)))))))))
