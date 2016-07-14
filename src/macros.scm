(define-syntax my-let
   (syntax-rules ()
      ((_ ((bound' form') (bound form) ...) first-body rest-body ...)
       ((lambda (bound' bound ...)
           first-body rest-body ...) form' form ...))
      ((_ var ((bound' form') (bound form) ...) first-body rest-body ...)
       ((_ var ((bound
