(load "00-common.scm")

(define add1
  (lambda (n)
    (+ n 1)))

(debug `(add1 42))


(define sub1
  (lambda (n)
    (- n 1)))

(debug `(sub1 42))


(define add
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (add n (sub1 m)))))))

(debug `(add 21 21))


(define sub
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (sub n (sub1 m)))))))

(debug `(sub 100 10))


(define mult
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (add n (mult n (sub1 m)))))))

(debug `(mult 4 3))


(define tup+
  (lambda (t1 t2)
    (cond
      ((null? t1) t2)
      ((null? t2) t1)
      (else (cons (add (car t1) (car t2)) (tup+ (cdr t1) (cdr t2)))))))

(debug `(tup+ '(1 2 3) '(4 5 6)))


(define gt
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (gt (sub1 n) (sub1 m))))))

(debug `(gt 123 2))
(debug `(gt 2 123))
(debug `(gt 123 123))


(define ge
  (lambda (n m)
    (cond
      ((zero? m) #t)
      ((zero? n) #f)
      (else (ge (sub1 n) (sub1 m))))))

(debug `(ge 123 2))
(debug `(ge 2 123))
(debug `(ge 123 123))


(define lt
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (lt (sub1 n) (sub1 m))))))

(debug `(lt 2 4))
(debug `(lt 4 2))
(debug `(lt 2 2))


(define le
  (lambda (n m)
    (cond
      ((zero? n) #t)
      ((zero? m) #f)
      (else (le (sub1 n) (sub1 m))))))

(debug `(le 2 4))
(debug `(le 4 2))
(debug `(le 2 2))


(define power
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (mult n (power n (sub1 m)))))))

(debug `(power 2 2))
(debug `(power 2 3))


(define div
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (div (sub n m) m))))))

(debug `(div 24 6))
(debug `(div 24 7))
(debug `(div 24 8))
(debug `(div 24 9))
(debug `(div 24 10))
(debug `(div 24 11))
(debug `(div 24 12))
(debug `(div 24 13))


(define eq
  (lambda (n m)
    (cond
      ((zero? n) (zero? m))
      ((zero? n) #f)
      (else (eq (sub1 n) (sub1 m))))))

(debug `(eq 21 21))
(debug `(eq 21 32))
(debug `(eq 21 (sub 32 11)))
