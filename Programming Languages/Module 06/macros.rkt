#lang racket

(define-syntax my-if
  (syntax-rules (then else)
    [(my-if e1 then e2 else e3)
     (if e1 e2 e3)]))

(define answer
  (lambda (x)
    (my-if x then 42 else 7)))

(println (answer #t))
(println (answer #f))


(define-syntax comment-out
  (syntax-rules ()
    [(comment-out ignore instead)
     instead]))

(comment-out
  (/ 3 0)
  (println 'ok))


; (define (my-delay th)
;   (mcons #f th))
;
; (define square (my-delay (lambda ()
;   (square 4))))
(define-syntax lazylambda
  (syntax-rules ()
    [(lazylambda thunk)
     (mcons #f (lambda () thunk))]))

(define (my-force p)
  (if (mcar p)
    (mcdr p)
    (begin (set-mcar! p #t)
           (set-mcdr! p ((mcdr p)))
           (mcdr p))))

(define square-four (lazylambda (* 4 2)))

(println (my-force square-four))
(println (my-force square-four))
(println (my-force square-four))
