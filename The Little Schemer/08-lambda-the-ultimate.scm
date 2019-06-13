(load "00-common.scm")

(define value
  (lambda (exp)
    (cond
      ((atom? exp) exp)
      ((eq? (car (cdr exp)) '+)
       (+ (value (car exp))
          (value (car (cdr (cdr exp))))))
      ((eq? (car (cdr exp)) '^)
       (expt (value (car exp))
             (value (car (cdr (cdr exp))))))
      ((eq? (car (cdr exp)) '*)
       (* (value (car exp))
          (value (car (cdr (cdr exp)))))))))

(debug `(value '1))
(debug `(value '(2 + 20)))
(debug `(value '(3 * (21 + 1))))
(debug `(value '(3 ^ 2)))


(define multirember
  (lambda (x xs)
    (cond
      ((null? xs) '())
      ((eq? x (car xs))
       (multirember x (cdr xs)))
      (#t (cons (car xs)
                (multirember x (cdr xs)))))))

(define multirember-f
  (lambda (test?)
    (lambda (x xs)
      (cond
        ((null? xs) '())
        ((test? x (car xs))
         ((multirember-f test?) x (cdr xs)))
        (#t (cons (car xs)
                  ((multirember-f test?) x (cdr xs))))))))

(define multirember-eq?
  (multirember-f eq?))

(debug `(multirember 2 '(1 2 3 4 5)))
(debug `(multirember-eq? 2 '(1 2 3 4 5)))
(debug `((multirember-f eq?) 2 '(1 2 3 4 5)))


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

; What does (multirember&co a lat f) do? It looks at every atom of the lat
; to see whether it is eq? to a. Those atoms that are not are collected in one
; list ls1; the others for which the answer is true are collected in a second
; list ls2. Finally, it determines the value of (f ls1 ls2).
(define multirember&co
  (lambda (x xs f)
    (cond
      ((null? xs) (f '() '()))
      ((eq? x (car xs))
       (multirember&co x (cdr xs)
                       (lambda (ls1 ls2)
                         (f ls1
                            (cons (car xs) ls2)))))
      (#t
       (multirember&co x (cdr xs)
                       (lambda (ls1 ls2)
                         (f (cons (car xs) ls1)
                            ls2)))))))

(debug '(multirember&co 'tuna '(strawberries tuna and swordfish)
                        (lambda (x y) (length x))))
