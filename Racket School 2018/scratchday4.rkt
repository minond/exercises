#lang turnstile/quicklang

(provide (rename-out [typed-datum #%datum]))

; (define-type-constructor -> #:arity 0)

(define-typerule typed-datum
  [(_ . n:integer) »
   -----
   [⊢ (#%datum . n) ⇒ Int]])

(define-typerule (typed-datum f e ...)
  [⊢ f » f- ⇒ (~-> tin ... tout)]
  [⊢ e » e- ⇒ tin]
  ---------------
  [⊢ (#%app f- e- ...) ⇒ tout])
