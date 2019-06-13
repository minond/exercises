#lang br/quicklang

(require brag/support
         racket/sequence)

(module+ reader
  (provide read-syntax))

(define lex
  (lexer
   ["%" 'taco]
   ["#$" null]
   [any-char (lex input-port)]))

;; (tokenize (open-input-string
;;   "##$%%%#$#$#$$##$%%%#$#$#$$##$%%%#$#$#$$##$%%%#$#$#$$##$%%%#$#$#$$##$%%%#$#$#$$##$%%%#$#$#$$##$%%%#$#$#$$##$%%%#$#$#$$##$%%%#$#$#$$"))

(define (tokenize ip)
  (define tokens (for/list ([tok (in-port lex ip)])
    tok))
  (sequence->list (in-slice 7 tokens)))

(define (parse toks)
  (for/list ([tok (in-list toks)])
    (integer->char
     (for/sum ([val (in-list tok)]
               [power (in-naturals)]
               #:when (eq? val 'taco))
       (expt 2 power)))))

(define (read-syntax src ip)
  (define toks (tokenize ip))
  (define parse-tree (parse toks))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module untaco racket
         (display (list->string 'PT))))))