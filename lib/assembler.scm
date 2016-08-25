(define instruction-table (make-hashtable))
(define instructions
  '(car cdr set-car! set-cdr! pair?
        + - * / exp
        vector vector-set! vector-ref vector? vector-length
        apply call tail-call return closure
        set
        load-constant load-argument load-environment load-global
        load-f load-t load-nil load-0 load-1
        store-environment store-argument store-global))

(do
    (
 (lambda (x) (hashtable-set! instruction-table (car x) (cadr x)))
 '((car 0)
   (cdr 1)
   (set-car! 2)
   (set-cdr! 3)
   (pair? 4)
   (+ 5)
   (- 6)
   (* 7)
   (/ 8)
   (exp 9)
   (vector 10)
   (vector-set! 11)
   (vector-ref 12)
   (vector? 13)
   (vector-length 14)
   (apply 15)
   (call 16)
   (tail-call 17)
   (return 18)
   (
  (define (assemble-instr port instr)
    (case (car instr)
      ((cons) (put-u8 port 0))
      ((car) (put-u8 port 1))
      ((cdr) (put-u8 port 2))
      ((set-car!) (put-u8 port 3))
      ((set-cdr!) (put-u8 port 4))
      ((pair?) (put-u8 port 5))
      ((+) (put-u8 port 6)
       (put-u8 port (cadr instr)))
      ((-) (put-u8 port 7))
      ((*) (put-u8 port 8))
      ((/) (put-u8 port 9))
      ((exp) (put-u8 port 10)
       (put-u8 port (cadr instr)))
      ((make-vector) (put-u8 port 11))
      ((vector-set!) (put-u8 port 12))
      ((vector-ref) (put-u8 port 13))
      ((vector?) (put-u8 port 14))
      ((vector-length) (put-u8 port 15))
      ((apply) (put-u8 port 16))
      ((tail-call) (put-u8 port 17))
      ((return) (put-u8 port 18))
      ((closure) (put-u8 port 19))
      ((set) (put-u8 port 20))
      ((load-constant) (put-u8 port 21))
      ((load-environment) (put-u8 port 22))
      ((load-argument) (put-u8 port 23))
      ((load-global) (put-u8 port 24))
      ((load-f) (put-u8 port 25))
      ((load-t) (put-u8 port 26))
      ((load-nil) (put-u8 port 27))
      ((load-0) (put-u8 port 28))
      ((load-1) (put-u8 port 29))
      ((store-environment) (put-u8 port 30))
      ((store-argument) (put-u8 port 31))
      ((store-global) (put-u8 port 32))))
  (define (assemble-bytecode bco)
    (let-values (((port to-bytevector) (open-bytevector-output-port)))
      (for-each (lambda (x) (assemble-instr port x)) bco)
      (to-bytevector))))





