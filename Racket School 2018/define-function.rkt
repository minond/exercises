#lang racket

(require (for-syntax syntax/parse))

(provide define-function
         function-app)

(define-syntax (define-function stx)
  (syntax-parse stx
    [(_ (function-name:id parameter:id ...) body:expr)
     (define arity (length (syntax->list #'(parameter ...))))
     #`(define-syntax function-name
         (cons #,arity #'(lambda (parameter ...) body)))]))

(define-syntax (function-app stx)
  (syntax-parse stx
    [(_ function-name:id argument:expr ...)
     #:do((define-values (arity function) (lookup #'function-name stx)))
     #:fail-unless (= arity (length (syntax->list #'(argument ...)))) "arity mismatch"
     #`(#,function argument ...)]))

(define-for-syntax (lookup function-name stx)
  (define (when-it-fails)
    (raise-syntax-error #f "not defined" stx))
  (define x (syntax-local-value function-name when-it-fails))
  (values (car x) (cdr x)))
