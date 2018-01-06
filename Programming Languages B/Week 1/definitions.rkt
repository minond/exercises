#lang racket

(provide (all-defined-out))

(define x 3)
(define y (+ x 2))

(define cube1
  (lambda (x)
    (* x (* x x))))

(define cube2
  (lambda (x)
    (* x x x)))

(define (cube3 x)
  (* x x x))

; x to the yth power
(define (pow1 x y)
  (if (= y 0)
    1
    (* x (pow1 x (- y 1)))))

(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))

(define pow3
  (lambda (y)
    (lambda (x)
      (pow1 x y))))

(define three-to-the (pow2 3))
(define squared (pow3 2))
(define cube4 (pow3 3))

(println (cube1 y))
(println (cube2 y))
(println (cube3 y))
(println (cube4 y))

(println (pow1 3 2))
(println (pow1 3 0))

(println (three-to-the 2))
(println (squared 3))
