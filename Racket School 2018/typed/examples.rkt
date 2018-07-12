#lang s-exp "lang.rkt"

5
#f

(+ 1 2)

(if #f 2 (+ 21 3))
(if #f #t #f)
(if (if #f #t #f) 2 3)
(if #t "true" "false")

(λ ([x : Number]) (+ x 12))
((λ ([x : Number]) (+ x 12)) 12)

(((λ ([x : String])
     (λ ([x : Number])
        (+ x x))) "s") 4)
