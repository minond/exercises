#lang s-exp "arith.rkt"

(define-function (add-them a b)
                 (+ a b))

(define-function (sub-them a b)
                 (- a b))

(if #t
  then (function-app add-them 1 12)
  else (function-app add-them 2 13))

(function-app sub-them 10 3)
