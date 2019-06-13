#lang racket

(require (for-syntax syntax/parse))

(provide #%module-begin #%top-interaction
         (rename-out [datum #%datum]
                     [app #%app]
                     [add +]))

;; NOTE #%quote and quote are the same. Racket's `'x` is exanded to (#%datum . x)
(define-syntax (datum stx)
  (syntax-parse stx
    [(_ . n:number) #''n] ; #'(#%datum . n)
    [(_ . x:string) #'(#%datum . x)]
    [_ (raise-syntax-error 'syntax-error "syntax not allowed" stx)]))

(define-syntax (app stx)
  (syntax-parse stx
    [(_) (raise-syntax-error 'application-error "empty application not allowed" stx)]
    [(x ...) #''(x ...)]))

(define-syntax (add stx)
  (syntax-parse stx
    [(~or (_ lhs:string rhs:expr)
          (_ lhs:expr rhs:string))
     (raise-syntax-error 'arg-error "string not allowed" stx)]
    [(_ lhs:expr rhs:expr) #`(+ lhs rhs)]))
