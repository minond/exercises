#lang racket

; Wraps a thunk in a promise-like mutable pair. The first item is a boolean
; flag that states whether the thunk has already been executed or not. If it
; has, the returned value is stored in the second item. If it hasn't, the thunk
; will be there instead.
(define (my-delay th)
  (mcons #f th))

(define (my-force p)
  (if (mcar p)
    (mcdr p)
    (begin (set-mcar! p #t)
           (set-mcdr! p ((mcdr p)))
           (mcdr p))))

(define (square x)
  (begin (println "running square procedure")
         (* x x)))

(define square-you-later (my-delay (lambda ()
  (square 4))))

(println square-you-later)
(println (my-force square-you-later))
(println (my-force square-you-later))
(println (my-force square-you-later))
(println (my-force square-you-later))
(println (square 4))
