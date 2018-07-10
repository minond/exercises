#lang racket

(require "../define-function.rkt"
         (for-syntax syntax/parse))

(provide #%module-begin then else
         define-function function-app
         (rename-out [number-datum #%datum]
                     [plus +]
                     [minus -]
                     [if2 if]
                     [complain-app #%app]))

#;
(module reader racket/base
  (provide (rename-out [my-read-syntax read-syntax]))

  (define (get-all src in)
    (define e (read-syntax src in))
    (if (eof-object? e)
      '()
      (cons e (get-all src in))))

  (define (my-read-syntax src in)
    (define all (get-all src in))
    #`(module whatever #,(datum->syntax #f 'arith)
        #,@all)))

; The syntax/module-reader language does everything the expression above does.
(module reader syntax/module-reader arith)

(define-syntax (number-datum stx)
  (syntax-parse stx
    [(_ . v:number) #'(#%datum . v)]
    [(_ . v:boolean) #'(#%datum . v)]
    [(_ . other) (raise-syntax-error #f "not allowed" #'other)]))

#;
(define-syntax (define-ops stx)
  (syntax-parse stx
   [(_ (name op) ...)
    #'(begin
        (define-syntax (name stx)
          (syntax-parse stx
            [(_ lhs rhs) #'(op lhs rhs)]))
        ...)]))

#;
(define-ops (plus +) (minus -))

#;
(define-syntax (define-and-provide-ops stx)
  (syntax-parse stx
    [(_ (name op) ...)
     #'(begin
         (begin
           (define-syntax (name stx)
             (syntax-parse stx
               [(_ lhs rhs) #'(op lhs rhs)]))
           (provide (rename-out [name op])))
         ...)]))

(define-syntax (define-and-provide-ops stx)
  (syntax-parse stx
    [(_ (name op) ...)
     #'(begin
         (define-syntax (name stx)
           (syntax-parse stx
             [(_ lhs rhs) #'(op lhs rhs)])) ...

         (provide (rename-out [name op])) ...)]))

(define-and-provide-ops (plus +) (minus -))
; (define-and-provide-ops + - * /)

#;
(define-syntax (plus stx)
  (syntax-parse stx
   [(_ n1 n2) #'(+ n1 n2)]))

;; Grammar
;; (if Expression then Expression else Expression)

#;
(define-syntax (if2 stx)
  (syntax-parse stx
    #:datum-literals (then else)
    [(_ cond:expr then t:expr else f:expr)
     #'(if cond t f)]))

(define-syntax (then stx)
  (raise-syntax-error 'not-allowed "then is not allower as an expression" stx))

(define-syntax (if2 stx)
  (syntax-parse stx
    #:literals (then else)
    [(_ cond:expr then t:expr else f:expr)
     #'(if cond t f)]))

(define-syntax (complain-app stx)
  (define (complain msg src-stx)
    (raise-syntax-error 'parentheses msg src-stx))
  (define without-app-stx
    (syntax-parse stx [(_ e ...) (syntax/loc stx (e ...))]))
  (syntax-parse stx
   [(_)
    (complain "empty parentheses are not allowed" without-app-stx)]
   [(_ n:number)
    (complain "extra parentheses are not allowed around numbers" #'n)]
   [(_ x:id _ ...)
    (complain "unknown operator" #'x)]
   [_
    (complain "something is wrong here" without-app-stx)]))
