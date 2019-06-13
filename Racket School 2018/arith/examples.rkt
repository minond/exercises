#lang arith

(define-function (add-them a b)
                 (+ a b))

(define-function (sub-them a b)
                 (- a b))

(if #t
  then (function-app add-them 1 12)
  else (function-app add-them 2 13))

(function-app sub-them 10 3)

"exactly"
(exactly .1)

"inexactly"
(inexactly .1)

"(inexactly (+ 1.0 (exactly (/ 1.0 2.0))))"
(inexactly (+ 1.0 (exactly (/ 1.0 2.0))))
"(exactly (inexactly (+ 1.0 (exactly (/ 1.0 2.0)))))"
(exactly (inexactly (+ 1.0 (exactly (/ 1.0 2.0)))))

(+ 0.1 0.1)
(+ 0.1 (+ 0.1 (+ 0.1 (+ 0.1 (+ 0.1 (+ 0.1 (+ 0.1 (+ 0.1 (+ 0.1 0.1)))))))))

(/ 2 3)
