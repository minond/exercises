#lang racket

; (struct foo (bar baz quux) #:transparent)
; Doing this defines a few functions in our environment:
;   - a foo function that takes a bar, a bar, and a quux
;   - a foo? function that returns true if we pass it a foo
;   - a foo-bar function that takes a foo and returns its bar
;   - a foo-baz function that takes a foo and returns its baz
;   - a foo-quux function that takes a foo and returns its quux

(provide (all-defined-out))

; The #:transparent attribute prints the struct's values in the repl.
; There are other attributes, like #:mutable, which makes structs mutable
; and defined even more functions in the environment such as set-foo-quux!
(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)

(define (eval-exp e)
  (cond [(const? e) e]
        [(negate? e) (const (- (const-int (eval-exp e))))]
        [(add? e) (let ([v1 (const-int (eval-exp (add-e1 e)))]
                        [v2 (const-int (eval-exp (add-e2 e)))])
                    (const (+ v1 v2)))]

        [(multiply? e) (let ([v1 (const-int (eval-exp (multiply-e1 e)))]
                             [v2 (const-int (eval-exp (multiply-e2 e)))])
                         (const (* v1 v2)))]))