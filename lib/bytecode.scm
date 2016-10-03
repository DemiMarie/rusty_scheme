;;;; bytecode.scm – bytecode objects and bytecode generation
;;;
;;; This library defines the type of bytecode objects.  It also defines
;;; operations for appending bytecodes.

(import
 (rnrs)
 (only (srfi :1) proper-list?)
 (srfi :9)
 (only (srfi :43) vector-copy)
 (only (srfi :69) make-hash-table hash-table-ref hash-table-set!))

(define-record-type :bco
  (make-bco len instrs consts consts-len stack-depth memo counter)
  bco?
  (len bco.len len-set!)
  (instrs bco.instrs instrs-set!)
  (consts bco.consts consts-set!)
  (consts-len bco.consts-len consts-len-set!)
  (stack-depth stack-depth stack-depth-set!)
  (memo memo)
  (counter counter counter-set!))

(define (create-bco)
  (make-bco 0 '#() '#(#f) 0 0 (make-hash-table) 0))

;; Add `object` to the constant vector of `bco`.
;; Returns the index of `object` in the constant vector of `bco`.
;; `object` must not be modified afterwords.
(define (add-to-constant-vector bco object)
  (assert (not (procedure? object)))
  (let ((constants (bco.consts bco))
        (bco-consts-len (bco.consts-len bco)))
    (assert (fixnum? bco-consts-len))
    (let ((capacity (vector-length constants)))
      (vector-set!
       (if (= bco-consts-len capacity)
           (let ((new-vector
                  (vector-copy constants
                               0
                               (+ 3 (* 2 capacity)) #f)))
             (consts-set! bco new-vector)
             new-vector)
           constants)
       bco-consts-len object)
      (consts-len-set! bco (+ 1 bco-consts-len))
      bco-consts-len)))

(define (emit bco . opcode)
  (assert (bco? bco))
  (let ((bytecode (bco.instrs bco))
        (bco-len (bco.len bco)))
    (let ((capacity (vector-length bytecode)))
      (vector-set!
       (cond
        ((= bco-len capacity)
         (let ((new-vector (vector-copy bytecode 0 (+ 3 (* 2 bco-len)) #f)))
           (instrs-set! bco new-vector)
           new-vector))
        ((< bco-len capacity)
         bytecode)
        (else
         (assert #f)))
       bco-len opcode)
      (len-set! bco (+ 1 bco-len))
      bco-len)))

(define (emit-stack-reset bco depth)
  (emit bco 'stack-reset depth)
  (stack-depth-set! bco depth))

(define (emit-load bco arg)
  (if (pair? arg)
      (case (cdr arg)
        ((argument) (emit bco 'load-argument (car arg)))
        ((global primitive) (emit bco 'load-global (car arg)))
        ((()) (emit bco 'load-environment (car arg)))
        (else (error 'assert "bad cdr of arg to be loaded" arg)))
      (emit bco 'load-environment arg)))

(define (emit-global bco symbol)
  (emit bco 'global-load symbol)
  (let ((new-depth (+ 1 (stack-depth bco))))
    (stack-depth-set! bco new-depth)
    new-depth))

;; Emit bindings.
;; Args: `bco` = bytecode object, `variables` = variables being bound
;; `env` = environment
(define (emit-bindings bco variables expressions env compile-form old-val)
  (assert (proper-list? variables))
  (assert (proper-list? expressions))
  (assert (= (length variables) (length expressions)))
  (let ((depth (stack-depth bco)))
    (for-each
     (lambda (var expr old-val)
       (compile-form expr)
       (if (> (stack-depth bco) depth)
           (emit bco 'adjust-stack (- (stack-depth bco) depth)))
       (set! depth (+ 1 depth))
       (stack-depth-set! bco depth)
       (hash-table-set! env var (cons depth old-val)))
     variables expressions old-val)
    depth))

(define (emit-variable-reference bco stack-position)
  (assert (bco? bco))
  (let ((new-stack-depth (+ 1 (stack-depth bco))))
    (stack-depth-set! bco new-stack-depth)
    (cond
     ((symbol? stack-position)
      (emit bco 'global-load stack-position))
     ((fixnum? stack-position)
      (emit bco 'load stack-position))
     (else (assert #f)))))

(define (emit-constant bco object)
  (case object
    ((#f) (emit bco 'load-f))
    ((#t) (emit bco 'load-t))
    ((0)  (emit bco 'load-0))
    ((1)  (emit bco 'load-1))
    (else
     (if (null? object)
         (begin
           (emit bco 'load-nil))
         ;; Memoize the objects using the bytecode object's memo table
         (let ((index (hash-table-ref (memo bco) object (lambda () #f))))
           (emit bco
                 'load-constant-index
                 (if index
                     index
                     (begin
                       (add-to-constant-vector bco object)
                       (let ((len (bco.consts-len bco)))
                         (hash-table-set! (memo bco) object len)
                         len)))))))))

(define (emit-set! bco stack-position)
  (cond
   ((symbol? stack-position)
    (emit bco 'global-load stack-position))
   ((fixnum? stack-position)
    (emit bco 'store-environment stack-position))
   ((pair? stack-position)
    (case (cdr stack-position)
      ((argument)
       (emit bco 'store-argument (car stack-position)))
      ((global)
       (emit bco 'store-global (car stack-position)))
      (else (assert #f))))
   (else
    (error 'assert "invalid stack position" stack-position))))

(define (emit-lambda-definition bco variadic? fixed-args body)
  (let ((stack-position (stack-depth bco))
        (label-start (incr-counter bco))
        (label-end (incr-counter bco)))
    (emit bco 'closure fixed-args variadic? label-end)
    (body)
    (emit bco 'label label-end)))

(define (incr-counter bco)
  (let ((old-val (counter bco)))
    (counter-set! bco (+ 1 old-val))
    old-val))

;; Emit a jump.
(define (emit-jump bco condition yes no)
  (let ((stack-position (+ 1 (stack-depth bco)))
        (label-true (incr-counter bco))
        (label-false (incr-counter bco)))
    (condition)
    (emit bco 'branch label-true)
    (no)
    (emit bco 'jump label-false)
    (emit bco 'label label-true)
    (yes)
    (emit bco 'label label-false)))
;;; Local Variables:
;;; mode: scheme
;;; End:
