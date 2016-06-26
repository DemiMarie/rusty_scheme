;: var=$1; shift; exec "$var" -- "$0" "$@"
;;(use-modules (sfri sfri-9))

;;;; bytecode.scm – bytecode objects and bytecode generation
;;;
;;; This library defines the type of bytecode objects.  It also defines
;;; operations for appending bytecodes and a DSL for assembling them.

(library
    (bytecomp bytecode)
  (import
   (srfi 9)
   (srfi 43))
  (export bytecode-object
          Cons
          Car
          Cdr
          Set-car!
          Set-cdr!
          Vector-set!
          Vector-ref
          Call
          TailCall
          Bytecode.new)
  (begin
    (define-record-type 'bytecode-object
      (make-bytecode-object bytecode bytecode-length constants constants-length)
      bytecode-object?
      (bytecode get-bytecode set-bytecode!)
      (bytecode-length get-bytecode-length set-bytecode-length!)
      (constants get-constants set-constants!)
      (constants-length get-constants-length set-constants-length!))

    #;(lambda (x)
        (syntax-case x ()
          ((_ bytecode-object (Cons register1 register2))
           (add-two-arg-opcode bytecode-object 'Cons register1 register2))
          ((_ bytecode-object (Car register1))
           (add-one-arg-opcode bytecode-object 'Car register1))
          ((_ bytecode-object (Cdr register1))
           (add-two-arg-opcode bytecode-object 'Cdr register1))
          ((_ bytecode-object (SetCar register1 register2))
           (add-two-arg-opcode bytecode-object 'SetCar register1 register2))
          ((_ bytecode-object (SetCar register1 register2))
           (add-two-arg-opcode bytecode-object 'SetCar register1 register2))
          ((_ bytecode-object (SetCar register1 register2))
           (add-two-arg-opcode bytecode-object 'SetCar register1 register2))
          ((_ bytecode-object (SetCar register1 register2))
           (add-two-arg-opcode bytecode-object 'SetCar register1 register2))
          ((_ bytecode-object (SetCar register1 register2))
           (add-two-arg-opcode bytecode-object 'SetCar register1 register2))
          ((_ bytecode-object (SetCar register1 register2))
           (add-two-arg-opcode bytecode-object 'SetCar register1 register2))
          ((_ bytecode-object (SetCar register1 register2))
           (add-two-arg-opcode bytecode-object 'SetCar register1 register2))
          ((_ bytecode-object (SetCar register1 register2))
           (add-two-arg-opcode bytecode-object 'SetCar register1 register2))))

    (define-syntax add-bytecode
      (syntax-rules ()
        ((_ bco (opcode arg))
         (add-one-arg-opcode bco 'opcode arg))
        ((_ bco (opcode arg1 arg2))
         (add-two-arg-opcode bco 'opcode arg1 arg2))))

    (define-syntax add-bytecodes
      (syntax-rules ()
        ((_ bytecode-object operations more-operations ...)
         (begin
           (add-bytecode bytecode-object operations)
           (add-bytecode bytecode-object more-operations) ...))))

    ;; Add `object` to the constant vector of `bco`.
    ;; Returns the index of `object` in the constant vector of `bco`.
    ;; `object` must not be modified afterwords.
    (define (add-to-constant-vector bco object)
      (let* ((constants (get-constants bco))
             (length-of-bco-constants (vector-length constants)))
        (let ((vector-length (get-constants-length bco)))
          (if (<= vector-length constants)
              (set-constants!
               bco (vector-copy constants 0 (* 2 vector-length) #f))))
        (set-constants-length! bco (+ 1 length-of-bco-constants))
        length-of-bco-constants))

    ;;; Emit a jump.
    (define (emit-jump bco))))

;;; Local Variables:
;;; mode: scheme
;;; End:
