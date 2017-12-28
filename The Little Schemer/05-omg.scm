(load "00-common.scm")

; How are insertR* and rember* similar?
;
; They both recur with the car, whenever the car is a list, as well as with the
; cdr.

; How are all *-functions similar?
;
; They all ask three questions and recur with the car as well as with the cdr,
; whenever the car is a list.

; Why?
;
; Because all *-functions work on lists that are either
;   - empty,
;   - an atom con'sed onto a list, or
;   - a list con'sed onto a list.

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(debug `(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup)))


(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(debug `(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood)))
