(load "00-common.scm")

(define lat?
  (lambda (xs)
    (cond
      ((null? xs) #t)
      ((atom? (car xs)) (lat? (cdr xs)))
      (else #f))))

(debug `(lat? '()))
(debug `(lat? '(1 2 3)))
(debug `(lat? '(1 2 3 (4))))
(debug `(lat? '(1 2 3 4 5)))
