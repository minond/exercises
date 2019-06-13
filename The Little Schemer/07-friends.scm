(load "00-common.scm")

(define member?
  (lambda (x xs)
    (cond
      ((null? xs) #f)
      ((eqan? x (car xs)) #t)
      (#t (member? x (cdr xs))))))

(debug `(member? 3 '(1 2 3 4 5)))
(debug `(member? 0 '(1 2 3 4 5)))


(define set?
  (lambda (xs)
    (cond
      ((null? xs) #t)
      ((member? (car xs) (cdr xs)) #f)
      (#t (set? (cdr xs))))))

(debug `(set? '(1 2 3 4)))
(debug `(set? '(4 2 3 4)))


(define makeset
  (lambda (xs)
    (cond
      ((null? xs) '())
      ((member? (car xs) (cdr xs)) (makeset (cdr xs)))
      (#t (cons (car xs) (makeset (cdr xs)))))))

(debug `(makeset '(1 2 3 4 5)))
(debug `(makeset '(1 1 1 1 2 3 4 5)))


(define subset?
  (lambda (xs ys)
    (cond
      ((null? xs) #t)
      ((null? ys) #f)
      (#t (and (member? (car xs) ys)
               (subset? (cdr xs) ys))))))

(debug `(subset? '(1 2 3) '(1 2 3 4 5 6)))
(debug `(subset? '(10 2 3) '(1 2 3 4 5 6)))
(debug `(subset? '(1 2 3) '(1 2)))
(debug `(subset? '(1 2 3) '()))
(debug `(subset? '() '()))


(define eqset?
  (lambda (xs ys)
    (and (subset? xs ys)
         (subset? ys xs))))

(debug `(eqset? '() '()))
(debug `(eqset? '(1 2 3) '(3 2 1)))
(debug `(eqset? '(1 2 3 4) '(3 2 1)))


(define interset?
  (lambda (xs ys)
    (cond
      ((null? xs) #f)
      (#t (or (subset? xs ys)
              (interset? (cdr xs) ys))))))

(debug `(interset? '(stewed tomatoes and macaroni) '(macaroni and cheese)))
(debug `(interset? '(1) '(2)))


(define interset
  (lambda (xs ys)
    (cond
      ((null? xs) '())
      ((null? ys) '())
      ((member? (car xs) ys)
       (cons (car xs) (interset (cdr xs) ys)))
      (#t (interset (cdr xs) ys)))))

(debug `(interset '(1 2 3 4 5 6) '(3 4 5 6 7 8 9 0)))
(debug `(interset '(1 2 3 4) '(5 6 7 8 9)))
(debug `(interset '() '(1 2 3)))
(debug `(interset '(1 2 3) '()))


(define union
  (lambda (xs ys)
    (cond
      ((null? xs) (makeset ys))
      (#t (union (cdr xs) (cons (car xs) ys))))))

(debug `(union '() '()))
(debug `(union '(1 2 3) '(1 2 3)))
(debug `(union '(1 2 3) '(4 5 6)))
(debug `(union '() '(1 2 3 4 5 6)))
(debug `(union '(1 2 3 4 5 6) '()))


(define intersetall
  (lambda (xs)
    (cond
      ((null? xs) '())
      ((null? (cdr xs)) (car xs))
      (#t (interset (car xs)
                    (intersetall (cdr xs)))))))

(debug `(intersetall '((1 2 3) (1 2) (1))))
(debug `(intersetall '((6 pears and)
                       (3 peaches and 6 peppers)
                       (8 pears and 6 plums)
                       (and 6 prunes with some apples))))


(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (#t #f))))

(debug `(a-pair? 1))
(debug `(a-pair? '(1)))
(debug `(a-pair? '(1 2)))
(debug `(a-pair? '(1 2 3)))

(define first
  (lambda (x)
    (car x)))

(define second
  (lambda (x)
    (car (cdr x))))

(define third
  (lambda (x)
    (car (cdr (cdr x)))))

(define build
  (lambda (x y)
    (cons x (cons y '()))))
