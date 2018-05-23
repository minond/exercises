(define debug
  (lambda (x)
    (begin
      (newline)
      (newline)
      (display x)
      (display " => ")
      (display (eval x user-initial-environment)))))

(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))

; Book has this as:
;
;   ((or (number? x) (number? y)) #f)
;   (else (eq? x y))
;
; Not sure which I like more.
(define eqan?
  (lambda (x y)
    (cond
      ((and (number? x) (number? y))
       (= x y))
      ((and (atom? x) (atom? y))
       (eq? x y))
      (else #f))))

(define (sub1 n)
  (- n 1))

(define (add1 n)
  (+ n 1))

(define (one? n)
  (= n 1))

(define (first xs) (car xs))
(define (second xs) (car (cdr xs)))
(define (third xs) (car (cdr (cdr xs))))
