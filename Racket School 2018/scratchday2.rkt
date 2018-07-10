#lang racket

(require (only-in racket/list
                  first
                  [second snd]))

(provide (rename-out [five my-fav-num]))

(define (my-fav-num)
  7)

(module right-away racket/base
  (#%module-begin
    7))

(require 'right-away)


(module parent racket/base
  (define y 123)
  (provide y)

  (module* child #;racket/base
    ; (require (submod ".."))
    (define x y)
    ; (provide x)
  )

  ; (require 'child)
  ; (displayln x)
  )

(require 'parent)
