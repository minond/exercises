#lang br/quicklang
(require "grammar.rkt")

(module+ reader
  (provide read-syntax))

(define (tokenize ip)
  (for/list ([tok (in-port read-char ip)])
    tok))

(define (parse toks)
  (define parse-tree-datum (parse-to-datum toks))
  (for/list ([leaf (in-list (cdr parse-tree-datum))])
    (integer->char
     (for/sum ([val (in-list (cdr leaf))]
               [power (in-naturals)]
               #:when (equal? val '(taco)))
       (expt 2 power)))))


(define (read-syntax src ip)
  (define toks (tokenize ip))
  (define parse-tree (parse toks))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module untaco racket
         (display (list->string 'PT))))))
