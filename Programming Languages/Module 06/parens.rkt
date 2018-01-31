#lang racket

(define (factorial n)
  (if (= 0 n)
    1
    (* n (factorial (- n 1)))))

(println (factorial 4))
(println (factorial 10))
