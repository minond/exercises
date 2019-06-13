#lang racket

(require (for-syntax syntax/parse))

(provide #%module-begin
         #%top-interaction
         (rename-out [datum #%datum]
                     [app #%app]))

(define-syntax (datum stx)
  (syntax-parse stx
    [(_ . d) #''taco]))

(define-syntax (app stx)
  (syntax-parse stx
    [(_ f args ...) #'`(taco ,args ...)]))

(module reader syntax/module-reader atomic-taco)