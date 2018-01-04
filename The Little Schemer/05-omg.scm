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


(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))))))

(debug `(insertL* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood)))


(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else
        (and (eqlist? (car l1) (car l2))
             (eqlist? (cdr l1) (cdr l2)))))))

(debug `(eqlist?
          '(1 2 3 4 5 6 7 8 9)
          '(1 2 3 4 5 6 7 8 9)))
