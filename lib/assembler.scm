(define instruction-table (make-hash-table))
(define instructions
  '(car cdr set-car! set-cdr! pair?
        + - * / exp
        vector vector-set! vector-ref vector? vector-length
        apply call tail-call return closure
        set
        load-constant load-argument load-environment load-global
        load-f load-t load-nil load-0 load-1
        store-environment store-argument store-global
        branch jump closure-extra bind-variable))
(let ((index 0))
  (for-each
   (lambda (x)
     (hash-table-set! instruction-table x index)
     (set! index (+ 1 index)))
   instructions))

;;; Assembles instruction `opcode` to binary port `port`.
;;; Returns the new offset relative to the start of the BCO.
(define (assemble-instr port opcode offset label-table)
  #;(pretty-print opcode)
  #;(newline)
  (define (put-u24 opvector)
    (map (lambda (x)
           (put-u8 port (logand x 255))
           (put-u8 port (logand (ash x -8) 255))
           (put-u8 port (logand (ash x -16) 255)))
         opvector))
  (assert (pair? opcode))
  (if (not (eq? (car opcode) 'label))
      (put-u8 port (hash-table-ref instruction-table (car opcode))))
  ;;(assert #f)
  (let ((op-vector
         (case (car opcode)
           ((load-f load-t load-nil load-0 load-1
                    cons car cdr
                    vector-ref vector-set!)
            '(0))
           ((load-global load-constant load-argument load-environment
                         bind-variable)
            (cdr opcode))
           ((closure jump branch)
            (let* ((opcode-list (cdr opcode))
                   (label-num
                    (begin
                      (car opcode-list)))
                   (label-list
                    (or (hash-table-ref label-table label-num (lambda ()
                                                                (list #f)))))
                   (tail (cdr label-list)))
              (set-cdr! label-list (cons label-num tail))
              (case (car opcode)
                ((closure)
                 (let ((fixed-args (car opcode-list)))
                   (assert (<= fixed-args (ash 1 23)))
                   (let
                       ((new-list
                         (list
                          (logior fixed-args
                                  (ash (if (cadr opcode-list) 0 1) 24)))))

                     (put-u24 new-list)
                     #f)
                   (put-u8 port
                           (hash-table-ref instruction-table 'closure-extra))
                   (cddr opcode-list)))
                ((branch jump) opcode-list)
                (else (assert #f)))))
           ((label)
            ;; Mark up a label
            (let* ((label-num (cadr opcode))
                   (label-list
                    (or (hash-table-ref label-table label-num
                                        (lambda () (list #f))))))
              (assert (not (car label-list)))
              (set-car! label-list offset)
              '()))
           (else
            ;; Can't happen
            (assert (not "Internal error: assembling invalid opcode"))))))
    (put-u24 op-vector))
   (if (eq? (car opcode) 'label)
       (+ offset 4)
       offset))

(define (fixup-offsets bytevec bco-table)
  (hash-for-each
   (lambda (key value)
     (let ((target (car value))
           (locations (cdr value)))
       (for-each
        (lambda (offset)
          (bytevector-uint-set! bytevec offset target (endianness little) 3))
        locations)))
   bco-table))

(define (assemble-bytecode bco)
  (let ((table (make-hash-table)))
    (let-values (((port to-bytevector)
                  (open-bytevector-output-port)))

      (define (assemble index offset instr)
        (assert (integer? index))
        (assert (integer? offset))
        (pretty-print index)
        (newline)
        (pretty-print offset)
        (newline)
        (pretty-print instr)
        (newline)
        (assemble-instr port instr offset table))
      #;(define (fold kons knil arg)
        (if (null? arg)
          knil
          (begin
            (fold (kons (car arg) knil) (cdr arg)))))
      (vector-fold assemble 0 bco)
      (to-bytevector))))
