(define (bound? sym) (symbol-bound? #f sym))
(define aset! vector-set!)
(define aref vector-ref)
(define (atom? obj) (not (pair? obj)))
