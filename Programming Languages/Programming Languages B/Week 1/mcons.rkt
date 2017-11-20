#lang racket

; mcons
; mcar
; mcdr
; mpair?
; set-mcar!
; set-mcdr!

(define x (mcons 1 (mcons 2 (mcons 3 4))))

(println x)
(println (mcar (mcdr x)))

(set-mcar! x 99)
(println x)

(println (mpair? x))
