(load "00-common.scm")

(define (noop-1 x) x)
(define (caadr xs) (car (car (cdr xs))))
(define (caar xs) (car (car xs)))
(define (cdadr xs) (cdr (car (cdr xs))))
(define (cdar xs) (cdr (car xs)))

(define (new-entry keys values)
  (cons keys (list values)))

(debug '(new-entry '(beverage dessert) '((food is) (number one))))

(define (lookup-in-entry name entry else-f)
  (cond
    ((null? entry) (else-f name))
    ((eqan? (caar entry) name) (caadr entry))
    (else (lookup-in-entry name (new-entry (cdar entry)
                                           (cdadr entry)) else-f))))

(debug '((lambda (entry) (new-entry (cdar entry) (cdadr entry)))
         (new-entry '(beverage dessert) '((food is) (number one)))))
(debug '(lookup-in-entry 'dessert
                         (new-entry '(beverage dessert) '((food is) (number one))) noop-1))
(debug '(lookup-in-entry 'entree
                         (new-entry '(appetizer entree beverage)
                                    '(food tastes good)) noop-1))


(define extend-table cons)

(define (lookup-in-table name table else-f)
  (lookup-in-entry
    name
    (car table)
    (lambda (name)
      (cond
        ((null? table) (else-f name))
        (else (lookup-in-table name (car table) else-f))))))

(define my-table (extend-table
                   (new-entry '(entree dessert) '(spaghetti spumoni))
                   (list (new-entry '(appetizer entryee beverage) '(food tastes good)))))

(debug 'my-table)
(debug '(lookup-in-table 'entree my-table noop-1))
