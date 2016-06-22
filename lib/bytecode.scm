                                        ;: var=$1; shift; exec "$var" -- "$0" "$@"
;;(use-modules (sfri sfri-9))

(library
    (bytecomp bytecode)
  (import (sfri 9))
  (export bytecode-object
          Cons
          Car
          Cdr
          Set-car!
          Set-cdr!
          Bytecode.new)
  (begin
    (define filename (car (command-line)))

    (define-record-type 'bytecode-object
      (make-bytecode-object bytecode bytecode-length constants constants-length)
      bytecode-object?
      (bytecode get-bytecode set-bytecode!)
      (bytecode-length get-bytecode-length set-bytecode-length!)
      (constants get-constants set-constants!)
      (constants-length get-constants-length set-constants-length!))

    (define-syntax add-bytecodes
      (syntax-rules (->)
        ((_ bytecode-object (token register1 register2) ...)
         (begin
           (add-bytecode bytecode-object 'token register1 register2)
           ...))))

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
