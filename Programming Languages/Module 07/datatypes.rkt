#lang racket

(provide (all-defined-out))

(define (funny-sum xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (funny-sum (cdr xs)))]
        [(string? (car xs)) (+ (string-length (car xs)) (funny-sum (cdr xs)))]))


(define (Const i) (list 'Const i))
(define (Negate e) (list 'Negate e))
(define (Add e1 e2) (list 'Add e1 e2))
(define (Multiply e1 e2) (list 'Multiply e1 e2))

(define (Const? ex) (eq? 'Const (car ex)))
(define (Negate? ex) (eq? 'Negate (car ex)))
(define (Add? ex) (eq? 'Add (car ex)))
(define (Multiply? ex) (eq? 'Multiply (car ex)))

(define (Const-int e) (car (cdr e)))
(define (Negate-e e) (car (cdr e)))
(define (Add-e1 e) (car (cdr e)))
(define (Add-e2 e) (car (cdr (cdr e))))
(define (Multiply-e1 e) (car (cdr e)))
(define (Multiply-e2 e) (car (cdr (cdr e))))

(define (eval-exp e)
  (cond [(Const? e) e]
        [(Negate? e) (Const (- (Const-int (eval-exp (Negate-e e)))))]
        [(Add? e) (let ([v1 (Const-int (eval-exp (Add-e1 e)))]
                        [v2 (Const-int (eval-exp (Add-e2 e)))])
                    (Const (+ v1 v2)))]

        [(Multiply? e) (let ([v1 (Const-int (eval-exp (Add-e1 e)))]
                             [v2 (Const-int (eval-exp (Add-e2 e)))])
                    (Const (* v1 v2)))]

        [#t (error "eval-exp expetected an expression")]))
