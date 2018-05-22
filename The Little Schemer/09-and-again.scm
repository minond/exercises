(load "00-common.scm")

(define pick
  (lambda (n xs)
    (cond
      ((zero? (- n 1)) (car xs))
      (#t (pick (- n 1) (cdr xs))))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn xs)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn xs) xs))
      (#t (eq? sorn a)))))

(define (C n)
  "Is this function total? It doesn't yield a value for 0, but otherwise
   nobody knows.

   Thank you, Lothar Collatz"
  (cond
    ((one? n) 1)
    ((even? n) (C (/ n 2)))
    (else (C (add1 (* n 3))))))

(define (A m n)
  "Does A always give an answer? Yes, it is total.

   Thank you, Wilhelm Ackermann"
  (cond
    ((zero? m) (add1 n))
    ((zero? n) (A (sub1 m) 1))
    (else (A (sub1 m)
             (A m (sub1 n))))))

((lambda (mk-length)
   (mk-length
     (mk-length
       (mk-length '()))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

(define (eternity)
  (eternity))

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((mk-length mk-length)
                     (cdr l))))))))
 '(apples 1 2 3 4 5 6))

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    ((lambda (length)
       (lambda (l)
         (cond
           ((null? l) 0)
           (else (add1 (length (cdr l)))))))
     (lambda (x)
       ((mk-length mk-length) x)))))
 '(apples 1 2 3 4 5 6 7 8 9))

(((lambda (le)
    ((lambda (mk-length)
       (mk-length mk-length))
     (lambda (mk-length)
       (le (lambda (x)
             ((mk-length mk-length) x))))))
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))
 '(apples 1 2 3 4 5 6 7 8 9))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(define my-length (Y
                    (lambda (length)
                      (lambda (l)
                        (cond
                          ((null? l) 0)
                          (else (add1 (length (cdr l)))))))))
