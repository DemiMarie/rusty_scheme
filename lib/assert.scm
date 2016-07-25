;; -*- scheme -*-
(library
   (assert)
   (export assert)
   (import)
   (define-syntax assert
      (syntax-rules ()
         ((_ asserted-form msg ...)
          (or asserted-form
              (error 'assert "assertion failed" 'asserted-form msg ...))))))
