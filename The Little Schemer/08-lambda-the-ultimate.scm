(load "00-common.scm")

(define eq?-c
  (lambda (a)
    (lambda (b)
      (equal? a b))))


(define multiremberT
  (lambda (f xs)
    (cond
      ((null? xs) '())
      ((f (car xs)) (multiremberT f (cdr xs)))
      (#t (cons (car xs)
                (multiremberT f (cdr xs)))))))

(define eq?-c
  (lambda (c)
    (lambda (x)
      (equal? x c))))

(define eq?-tuna
  (eq?-c 'tuna))

(debug '(multiremberT eq?-tuna '(shrimp salad tuna sald and tuna)))


(define evens-only*
  (lambda (xs)
    (cond
      ((null? xs) '())
      ((atom? (car xs))
       (cond
         ((even? (car xs)) (cons (car xs) (evens-only* (cdr xs))))
         (#t (evens-only* (cdr xs)))))
      (#t (cons (evens-only* (car xs))
                (evens-only* (cdr xs)))))))

(debug '(even? 0))
(debug '(even? 1))
(debug '(even? 2))
(debug '(even? 3))
(debug '(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)))
