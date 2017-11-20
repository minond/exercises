#lang racket

; If the file that defines a top-level binding and does not `set!` it, then no
; other file can either.

(define x (begin
  (+ 1 2)
  (* 2 5)
  (/ 9 3)
  (- 5 2)))

(define y (let ([x 1])
  (begin (set! x 5) x)))

(println x)
(println y)


(define name "Marcos")
(define get-name (lambda () name))
(set! name "Marcos Minond")

(println (get-name))


(define two 2)

(define (add-two-a n)
  (+ two n))

(define add-two-b
  (let ([two two]
        [+ +])
    (lambda (n)
      (+ two n))))

(set! two 3)

(println (add-two-a 3))
(println (add-two-b 3))
