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

; Types:
; *const
; *quote
; *identifier
; *lambda
; *cond
; *application

(define (build typ env)
  (cons typ (list env)))

(define (expression-to-action e)
  (cond
    ((atom? e) (atom-to-action e))
    (else (list-to-action e))))

(define (atom-to-action e)
  (cond
    ((number? e) *const)
    ((eq? e #t) *const)
    ((eq? e #f) *const)
    ((eq? e 'cons) *const)
    ((eq? e 'car) *const)
    ((eq? e 'cdr) *const)
    ((eq? e 'null?) *const)
    ((eq? e 'eq?) *const)
    ((eq? e 'atom?) *const)
    ((eq? e 'zero?) *const)
    ((eq? e 'add1) *const)
    ((eq? e 'sub1) *const)
    ((eq? e 'number?) *const)
    (else *identifier)))

(define (list-to-action e)
  (cond
    ((atom? (car e))
     (cond
       ((eq? (car e) 'lambda) *lambda)
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) 'cond) *cond)
       (else *application)))
    (else *application)))

; What makes value unusual? It sees representations of expressions.
(define (value e)
  (meaning e '()))

(define (meaning e table)
  ((expression-to-action e) e table))

(define (*const e table)
  (cond
    ((number? e) e)
    ((eq? e #t) #t)
    ((eq? e #f) #f)
    (else (build 'primitive e))))

(define (*quote e table)
  (unquote e))

(define (*identifier e table)
  (lookup-in-table e table (lambda (e)
                             (car '()))))

(define table-of first)
(define formals-of second)
(define body-of third)
(define (*lambda e table)
  (build 'non-primitive (cons table (cdr e))))

(define question-of first)
(define answer-of second)
(define cond-lines-of cdr)

(define (evcond lines table)
  (cond
    ((else? (question-of (car lines)))
     (meaning (answer-of (car lines)) table))
    ((meaning (question-of (car lines)))
     (meaning (answer-of (car lines))))
    (else (evcond (cdr lines) table))))

(define (else? c)
  (and (atom? c) (eq? c 'else)))

(define (*cond e table)
  (evcond (cond-lines-of e) table))

(define (evlis es table)
  (cond
    ((null? es) '())
    (else (cons (meaning (car es) table)
                (evlis (cdr es) table)))))

(define function-of car)
(define arguments-of cdr)

(define (primitive? x)
  (eq? 'primitive (first x)))

(define (non-primitive? x)
  (eq? 'non-primitive (first x)))

(define (apply f args)
  (cond
    ((primitive? f) (apply-primitive (second f) args))
    ((non-primitive? f) (apply-closure (second f) args))))

(define (apply-primitive name args)
  (cond
    ((eq? name 'cons) (cons (first args) (second args)))
    ((eq? name 'car) (car (first args)))
    ((eq? name 'cdr) (cdr (first args)))
    ((eq? name 'null?) (null? (first args)))
    ((eq? name 'eq?) (eq? (first args)))
    ((eq? name 'atom?) ((lambda (x)
                          (cond
                            ((null? x) #f)
                            ((atom? x) #t)
                            ((primitive? x) #t)
                            ((non-primitive? x) #t)
                            (else #f))) (first args)))
    ((eq? name 'zero?) (zero? (first args)))
    ((eq? name 'number?) (number? (first args)))
    ((eq? name 'add1) (add1 (first args)))
    ((eq? name 'sub1) (sub1 (first args)))))

(define (apply-closure closure args)
  (meaning (body-of closure)
           (extend-table (new-entry (formals-of closure) args)
                         (table-of closure))))

(define (*application e table)
  (apply (meaning (function-of e) table)
         (evlis (arguments-of e) table)))
