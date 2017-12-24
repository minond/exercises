(load "common.scm")

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define add
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (add n (sub1 m)))))))

(define sub
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (sub n (sub1 m)))))))

(define mult
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (add n (mult n (sub1 m)))))))

(debug `(add1 42))
(debug `(sub1 42))
(debug `(add 21 21))
(debug `(sub 100 10))
(debug `(mult 4 3))
(quit)
