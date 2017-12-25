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

(define power
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (mult n (power n (sub1 m)))))))

(define div
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (div (sub n m) m))))))
