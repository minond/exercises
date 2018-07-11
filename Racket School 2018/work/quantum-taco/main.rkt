#lang br/quicklang

(module+ reader
  (provide read-syntax))

(define (tokenize ip)
  (define c (read ip))
  (if (eof-object? c)
      '()
      (cons c (tokenize ip))))

(define (parse tok)
  (if (cons? tok)
      (map parse tok)
      'taco))

(define (read-syntax src ip)
  (define toks (tokenize ip))
  (define parse-tree (parse toks))
  (with-syntax ([(PT ...) parse-tree])
    #'(module tacofied racket
        'PT ...)))