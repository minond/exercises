#lang racket

; A list ending in `null` is a proper list. Otherwise it's refered to as an
; improper list.

(println (list? null))
(println (list? (list 1 2 3)))
(println (list? (cons 1 (cons 2 3))))
(println (list? (cons 1 (cons 2 null))))

(println (pair? null))
(println (pair? (list 1 2 3)))
(println (pair? (cons 1 (cons 2 3))))
(println (pair? (cons 1 (cons 2 null))))
