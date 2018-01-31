#lang racket

; A stream is an infinite sequences of values. The stream produces *knows* how
; to create as many values as are needed. The stream consumer knows how many
; values to ask for. Streams can be represented as thunks, that when called,
; return a pair where the car is the value and the cdr is the next stream.
(define ones (lambda ()
  (cons 1 ones)))

(println (car (ones)))
(println (car ((cdr (ones)))))
(println (car ((cdr ((cdr (ones)))))))
(println (car ((cdr ((cdr ((cdr (ones)))))))))


(define numbers
  (letrec ([f (lambda (n) (cons n (lambda () (f (+ n 1)))))])
    (lambda () (f 1))))

(println (car (numbers)))
(println (car ((cdr (numbers)))))
(println (car ((cdr ((cdr (numbers)))))))
(println (car ((cdr ((cdr ((cdr (numbers)))))))))
(println (car ((cdr ((cdr ((cdr ((cdr (numbers)))))))))))
(println (car ((cdr ((cdr ((cdr ((cdr ((cdr (numbers)))))))))))))
(println (car ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr (numbers)))))))))))))))
(println (car ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr (numbers)))))))))))))))))
(println (car ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr (numbers)))))))))))))))))))
(println (car ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr (numbers)))))))))))))))))))))


(define powers-of-two
  (letrec ([f (lambda (n)
      (let ([n2 (* n 2)])
        (cons n2 (lambda () (f n2)))))])
    (lambda () (f 1))))

(println "powers-of-two")
(println (car (powers-of-two)))
(println (car ((cdr (powers-of-two)))))
(println (car ((cdr ((cdr (powers-of-two)))))))
(println (car ((cdr ((cdr ((cdr (powers-of-two)))))))))
(println (car ((cdr ((cdr ((cdr ((cdr (powers-of-two)))))))))))
(println (car ((cdr ((cdr ((cdr ((cdr ((cdr (powers-of-two)))))))))))))
(println (car ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr (powers-of-two)))))))))))))))
(println (car ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr (powers-of-two)))))))))))))))))
(println (car ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr (powers-of-two)))))))))))))))))))
(println (car ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr ((cdr (powers-of-two)))))))))))))))))))))
