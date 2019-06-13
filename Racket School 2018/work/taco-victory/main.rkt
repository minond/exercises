#lang br/quicklang

(require brag/support "grammar.rkt")
(provide (all-from-out br/quicklang) (all-defined-out))

(module+ reader
  (provide read-syntax))

(define (tokenize ip)
  (define lex
    (lexer
     ["#$" lexeme]
     ["%" lexeme]
     [any-char (lex input-port)]))
  (lex ip))

(define (taco-program . pieces)
  pieces)

(define (taco-leaf . pieces)
  (integer->char
   (for/sum ([taco-or-not (in-list pieces)]
             [power (in-naturals)])
     (* taco-or-not (expt 2 power)))))

(define (taco)
  1)

(define (not-a-taco)
  0)

(define (read-syntax src ip)
  (define token-thunk (λ () (tokenize ip)))
  (define parse-tree (parse token-thunk))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module winner taco-victory
         (display (apply string PT))))))