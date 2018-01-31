#lang racket

; Thunk: a zero argument function whose purpose is to delay evaluation. To
; "thunk" the expression is to wrap an evaluation in a function.

(define (fact-1 x)
  (if (= x 0)
      1
      (* x (fact-1 (- x 1)))))

(define (if-alt check yes no)
  (if check
      (yes)
      (no)))

(define (fact-2 x)
  (if-alt (= x 0)
    (lambda () 1)
    (lambda () (* x (fact-2 (- x 1))))))

(println (fact-1 500))
(println (fact-2 500))

(println ((lambda () 42)))
