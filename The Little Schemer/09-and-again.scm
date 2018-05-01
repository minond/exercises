(load "00-common.scm")

; (6 3 4 caviar 7 6)
; (6 3 tuna caviar 7 6)

(define pick
  (lambda (n xs)
    (cond
      ((zero? (- n 1)) (car xs))
      (#t (pick (- n 1) (cdr xs))))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))
