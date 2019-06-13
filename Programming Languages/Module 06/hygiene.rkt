#lang racket

(define-syntax dbl
  (syntax-rules ()
    [(dbl x)
     (let ([y 1])
          (* 2 x y))]))

; Macro is expanded to:
;
;   (define x1 (let ([y 1])
;                   (* 2 4 y)))
(define x1 (dbl 4))

; Marco is expanded to:
;
;   (define x2 (let [(y 4)]
;                   (let ([y 1])
;                        (* 2 y y))))
;
; BUT both `y`'s are bound the their correct scope. This is what is mean by
; hygienic macros. Lexical scope but for macros.
(define x2 (let [(y 4)]
                (dbl y)))

(println x1)
(println x2)
