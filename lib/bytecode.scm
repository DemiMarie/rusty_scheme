;;(use-modules (sfri sfri-9))

;;;; bytecode.scm – bytecode objects and bytecode generation
;;;
;;; This library defines the type of bytecode objects.  It also defines
;;; operations for appending bytecodes and a DSL for assembling them.

(library
    (bytecode)
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
          emit-lambda-definition
          add-to-constant-vector
          create-bco
          emit-jump
          emit-constant
          emit-set!
          emit-primitive)
  (import
   (rnrs)
   ;(rnrs records syntactic (6))
   (srfi :9)
   (srfi :43)
   (srfi :69))
  #;(define vector-copy #f)
  (define-record-type :bco
    (make-bco len instrs consts consts-len stack-depth memo)
    bco?
    (len len len-set!)
    (instrs instrs instrs-set!)
    (consts consts consts-set!)
    (consts-len consts-len consts-set!)
    (stack-depth stack-depth stack-depth-set!)
    (memo memo))

  #;(define-record-type
    (bco make-bco bco?)
    (fields
     (mutable len len len-set!)
     (mutable instrs instrs instrs-set!)
     (mutable consts consts consts-set!)
     (mutable consts-len consts-len consts-len-set!)
     (mutable stack-depth stack-depth stack-depth-set!)
    (sealed #t)
    (opaque #t)))
  (define (create-bco)
    (make-bco 0 #() #(#f) 0 0 (make-hash-table)))

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
    (let* ((constants (consts bco))
           (length-of-bco-constants (vector-length constants)))
      (let ((vector-length (consts-len bco)))
        (if (= vector-length length-of-bco-constants)
            (let ((new-vector (vector-copy constants 0 (* 2 vector-length) #f)))
              (vector-set! new-vector vector-length object)
              (consts-set! bco new-vector)))
        (len-set! bco length-of-bco-constants))
      length-of-bco-constants))

  (define (emit bco . opcode)
    (or (bco? bco) (error 'assert "wrong type to emit"))
    (let ((bytecode (instrs bco))
           (bco-len (len bco)))
      (let ((capacity (vector-length bytecode)))
        (if (= bco-len capacity)
            (let ((new-vector
                   (vector-copy bytecode 0 (+ 1 (* 2 bco-len)) #f)))
              (vector-set! new-vector bco-len bytecode)
              (instrs-set! bco new-vector)))
        (len-set! bco (+ 1 bco-len))
        len)))

  (define (emit-primitive bco prim args)
    (for-each
     (lambda (x) (emit 'load x))
     args)
    (emit prim))

  (define (emit-constant bco object)
    (case object
      ((#f) (emit bco 'load-f))
      ((#t) (emit bco 'load-t))
      ((0)  (emit bco 'load-0))
      ((1)  (emit bco 'load-1))
      (else ; Memoize the objects using the bytecode object's memo table
       (let ((index (hash-table-ref (memo bco) #f)))
         (emit bco 'load-constant-index
               (if index index (add-to-constant-vector bco object)))))))

  (define (emit-set! bco stack-position other-stack-position)
    (cond
     ((symbol? stack-position)
      (emit bco 'global-load stack-position))
     ((fixnum? stack-position)
      (emit bco 'load stack-position))
     (else (error 'assert "invalid stack position"))))

  (define counter 0)

  (define (emit-lambda-definition bco variadic? fixed-args body)
    (let ((stack-position (stack-depth bco))
          (label-start (incr-counter))
          (label-end (incr-counter)))
      (emit bco 'label label-start)
      (body)
      (emit bco 'label label-end)
      (emit bco 'closure label-start label-end variadic? fixed-args)))

  (define (incr-counter)
    (let ((old-val counter))
      (set! counter (+ 1 counter))
      old-val))
  ;; Emit a jump.
  (define (emit-jump bco condition yes no)
    (let ((stack-position (stack-depth bco))
          (label-true (incr-counter))
          (label-false (incr-counter)))
      (condition)
      (emit bco 'adjust-stack stack-position)
      (emit bco 'branch label-true label-false)
      (emit bco 'label label-true)
      (yes)
      (emit bco 'label label-false)
      (no))))
;;; Local Variables:
;;; mode: scheme
;;; End:
