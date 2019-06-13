#lang racket

(struct obj (fields methods))

(define (assoc-m v xs)
  (cond [(null? xs) #f]
        [(equal? v (mcar (car xs))) (car xs)]
        [#t (assoc-m v (cdr xs))]))

(define (get obj field)
  (let ([f (assoc-m field (obj-fields obj))])
    (if f
      (mcdr f)
      (error "error looking up field" field))))

(define (set obj field val)
  (let ([f (assoc-m field (obj-fields obj))])
    (if f
      (set-mcdr! f val)
      (error "error looking up field" field))))

(define (send obj msg . args)
  (let ([method (assoc msg (obj-methods obj))])
    (if method
      ((cdr method) obj args)
      (error "error looking up method" method))))

(define (make-point x y)
  (obj
    (list (mcons 'x x)
          (mcons 'y y))
    (list (cons 'get-x (lambda (self args) (get self 'x)))
          (cons 'get-y (lambda (self args) (get self 'y)))
          (cons 'set-x (lambda (self args) (set self 'x (car args))))
          (cons 'set-y (lambda (self args) (set self 'y (car args))))
          (cons 'mult (lambda (self args)
                        (* (send self 'get-x)
                           (send self 'get-y)))))))

(define (make-point-two x y z)
  (let ([parent (make-point x y)])
    (obj
      (append
        (list (mcons 'z z))
        (obj-fields parent))
      (append
        (list (cons 'get-z (lambda (self args) (get self 'z)))
              (cons 'get-x (lambda (self args) (get self 'z))))
        (obj-methods parent)))))


(define p (make-point 32 24))
(send p 'set-y 23)
(println (send p 'get-x))
(println (send p 'get-y))
(println (send p 'mult))

(define p2 (make-point-two 32 24 7))
(send p 'set-y 23)
(println (send p2 'get-x))
(println (send p2 'get-y))
(println (send p2 'get-z))

; This makes a call to make-point's mult method which in turn calls
; make-point-two's get-x and get-y methods. Just like in Ruby.
(println (send p2 'mult))
