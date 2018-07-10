#lang s-exp "define-function.rkt"

(define-function (s a b)
                 (+ a b))

(function-app s 1 12)

; #lang racket
;
; (require "define-function.rkt")
;
; (define-function (s a b)
;                  (+ a b))
;
; (function-app s 1 12)
