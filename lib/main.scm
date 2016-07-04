;;; -*- scheme -*-
(import
   (rnrs)
   (tree-walk)
   (environment)
   (bytecode))

(compile-form
   '(letrec ((alpha #f)) (write alpha))
   (make-environment) (create-bco))
