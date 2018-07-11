#lang br/quicklang

(module+ reader
  (provide read-syntax))

(define (char-to-tacos c)
  (define v (char->integer c))
  (build-list v (lambda (x) 'taco)))

(define (char-to-spaces c)
  (define v (char->integer c))
  (build-string v (lambda (x) #\ )))

(define (tokenize ip)
  (define c (read-char ip))
  (if (eof-object? c)
      '()
      (cons c (tokenize ip))))

(define (parse f toks)
  (map f toks))

(define (read-syntax src ip)
  (define toks (tokenize ip))
  (define parse-tree-spaces (parse char-to-spaces toks))
  (define parse-tree-tacos (parse char-to-tacos toks))
  (strip-context
   (with-syntax ([(PT-space ...) parse-tree-spaces]
                 [(PT-taco ...) parse-tree-tacos])
     #`(module tacofied racket
         (displayln 'PT-taco) ...))))