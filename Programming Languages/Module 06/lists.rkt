#lang racket

(provide (all-defined-out))

; Empty list: null
; Cons constructor: cons
; Access head of list: car
; Access tail of list: cdr
; Check for emtpy: null?
;
; Notes:
; - () doesn't work for null, but '() does
; - (list e1 ... en) for building lists
; - Names car and cdr are historical acceidents

; sum all numbers in a list
(define (sum xs)
  (if (null? xs)
    0
    (+ (car xs) (sum (cdr xs)))))

; append function
(define (my-append xs ys)
  (if (null? xs)
    ys
    (cons (car xs) (my-append (cdr xs) ys))))

; map function
(define (my-map f xs)
  (if (null? xs)
    null
    (cons (f (car xs)) (my-map f (cdr xs)))))

(println (= 5 (sum (list 1 1 1 1 1))))
(println (my-append (list 1 2 3) (list 4 5 6)))
(println (my-map (lambda (x) (* 2 x)) (list 1 2 3 4)))
