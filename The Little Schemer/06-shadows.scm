(load "00-common.scm")

(define value
  (lambda (exp)
    (cond
      ((atom? exp) exp)
      ((eq? (car (cdr exp)) '+)
       (+ (value (car exp))
          (value (car (cdr (cdr exp))))))
      ((eq? (car (cdr exp)) '*)
       (* (value (car exp))
          (value (car (cdr (cdr exp)))))))))

(debug `(value '1))
(debug `(value '(2 + 20)))
(debug `(value '(3 * (21 + 1))))


(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(debug '(sero? '()))
(debug '(sero? '(() ())))
(debug '(edd1 '(() ())))
(debug '(zub1 '(() ())))
(debug '(edd1 (edd1 '(() ()))))
(debug '(edd1 (edd1 (edd1 '(() ())))))


(define lat?
  (lambda (xs)
    (cond
      ((null? xs) #t)
      ((atom? (car xs)) (lat? (cdr xs)))
      (else #f))))

(debug `(lat? '(1 2 3)))                    ; #t, of course
(debug `(lat? '((()) (() ()) (() () ()))))  ; #f, you must beware of shadows.
