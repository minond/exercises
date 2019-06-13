#lang racket

; (cond [e1a e1b]
;       [e2a e2b]
;       [true eNb])

(define (sum1 xs)
  (if (null? xs)
    0
    (if (list? (car xs))
      (+ (sum1 (car xs)) (sum1 (cdr xs)))
      (+ (car xs) (sum1 (cdr xs))))))

(define (sum2 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum2 (cdr xs)))]
        [#t (+ (sum2 (car xs)) (sum2 (cdr xs)))]))

(define (count-falses xs)
  (cond [(null? xs) 0]
        [(car xs) (count-falses (cdr xs))]
        [#t (+ 1 (count-falses (cdr xs)))]))

(println (sum1 (list 1 2 3 4 (list 1 1 1))))
(println (sum2 (list 1 2 3 4 (list 1 1 1))))
(println (count-falses (list 1 2 3)))
(println (count-falses (list 1 2 3 '() #t (list 1) (list) #f)))
(println (count-falses (list 1 #f #f '() #t (list 1) (list) #f)))
