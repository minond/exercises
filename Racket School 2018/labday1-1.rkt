#lang racket

(require (for-syntax racket/list))
(require (for-syntax syntax/parse))

(define-syntax (define-world* stx)
  (syntax-parse stx
    [(_ identifier:id ...)
     #`(begin
         (define value -1)
         (define identifier (begin (set! value (+ value 1)) value)) ...)]))


#;
(define-world* x y z)


(define-for-syntax (pairings xs n)
  (cond [(empty? xs) '()]
        [else (cons ; (cons (car xs) n)
                    #`(define #,(car xs) #,n)
                    (pairings (cdr xs) (+ n 1)))]))

#;
(define-syntax (define-world** stx)
  (define parts (syntax->list stx))
  (define ids (cdr parts))
  (define pairs (pairings ids 0))
  #`(begin #,@pairs))

#;
(define-world** x y z)

(define-syntax (define-rewrite-rule stx)
  (syntax-parse stx
    [(_ (the-name pattern ...) template)
     #'(define-syntax (the-name stx)
         (syntax-parse stx
           [(_ pattern ...) #'template]))]))


(define-rewrite-rule (ident x) x)
(define-rewrite-rule (sum-of x ...) (+ x ...))
