#lang racket

; let - Uses the environment *before* the let expression.
; let* - Uses the environment built up before the binding. Similar to SML.
; letrec - Uses the environment built before and after the binding.
; define - Similar to letrec, recommended by Racket community.

(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list given an empty list")]
        [(null? (cdr xs)) (car xs)]
        [#t (let ([tmax (max-of-list (cdr xs))])
              (if (> tmax (car xs)) tmax (car xs)))]))

(println (max-of-list (list 1 2 3 4 5 4 3 2 1)))
