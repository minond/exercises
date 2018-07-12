#lang racket

;; Terms
;; - stratification

(require racket/list)
(require rackunit)
(require (for-syntax racket/list))
(require (for-syntax syntax/parse))

#;
(define-syntax (f x)
  ; (displayln (syntax-line x))
  ; (displayln (syntax-e x))
  ; (displayln (syntax->list x))
  (define e (syntax->list x))
  (second e))

; (f 17)
; (f 17 32 05)
; (f (+ 1 0) (f 1))
; (f (second '(1 2 3)))

;; SYNTAX
;; (define-hello world) mean the identifier 'world' is bould to "good bye"
;; translate this code of the shape (define 2nd-of-given-syntax "good bye")

#;
(define-syntax (define-hello stx)
  (syntax-parse stx
    [(_ the-identifier:id ...)
     #`(begin
         (define the-identifier "good bye")
         ...)]))

;; SYNTAX
;; produces the list of all non-#false results of the give expressions
;; until it encounters #false the syntax indicated that there is at least
;; one expression though there might be arbitrarily many
;; if the first expression yield #f then (some ...) returns #f

#;
(define-syntax (some stx)
  (syntax-parse stx
    [(_ e0:expr)
     #`(let ([e0-value e0])
         (and e0-value (list e0-value)))]
    [(_ e0:expr e1:expr ...)
     #'(combine e0 (some e1 ...))]))

#;
(define-syntax (combine stx)
  (syntax-parse stx
    [(_ e0 some-of-e1)
     #`(let ([v e0])
         (if v
             (let ([w some-of-e1])
               (if (cons? w)
                   (cons v w)
                   (list v)))
             #f))]))

#;
(define-syntax (define-world* stx)
  (syntax-parse stx
    [(_ identifier:id ...)

       #`(begin
           (define value -1)
           (define identifier (begin (set! value (+ value 1)) value)) ...)]))

(define (dummy-extract tags from to)
  (list tags from to))

(begin-for-syntax
  (define-syntax-class byte
    (pattern n:number
             #:fail-unless (<= (syntax-e #'n) 256) "bad byte")))

#;
(define-syntax (split-ct stx)
  (syntax-parse stx
    [(_ tags start:byte end:byte [name step:byte (~optional converter)] ...)
     #:do ((define width 98)
           (define end-number (syntax-e #'end)))
     #:fail-unless (<= width end-number) "index out of range"
     #'(let*-values ([(index) start]
                     [(name index) (values (dummy-extract tags index (+ index step -1))
                                           (+ index step))]
                     ...)
         (values (~? (converter name) name) ...))]))

#;
(split-ct 'tags 0 128
          [title 10]
          [artist 30]
          [album 30]
          [year 4])
#;
(define-syntax (split-ct stx)
  (syntax-parse stx
    [(_ tags start end [name step (~optional converter)] ...)
     #'(let*-values ([(index) start]
                     [(name index) (values (dummy-extract tags index (+ index step -1))
                                           (+ index step))]
                     ...)
         (values (~? (converter name) name) ...))]))

#;
(split-ct 'tags 0 128
          [title 10]
          [artist 30]
          [album 30]
          [year 4 (lambda (x) (cons 'hi x))])

#;
(define-syntax (define-function stx)
  (syntax-parse stx
    [(_ (function-name:id parameter:id ...) body:expr)
     (define arity (length (syntax->list #'(parameter ...))))
     #`(define-syntax function-name
         (cons #,arity #'(lambda (parameter ...) body)))]))

#;
(define-syntax (function-app stx)
  (syntax-parse stx
    [(_ function-name:id argument:expr ...)
     #:do((define-values (arity function) (lookup #'function-name stx)))
     #:fail-unless (= arity (length (syntax->list #'(argument ...)))) "arity mismatch"
     #`(#,function argument ...)]))

#;
(define-for-syntax (lookup function-name stx)
  (define (when-it-fails)
    (raise-syntax-error #f "not defined" stx))
  (define x (syntax-local-value function-name when-it-fails))
  (values (car x) (cdr x)))

#;
(define-function (s a b)
                 (+ a b))

; (function-app s 1 2 3) ; => arity mismatch
; (function-app s2 1 2) ; => syntax-local-value: unbound identifier: #<syntax:readline-input:115:14 s2>
; (function-app s 1 2)

#;
(define-syntax (define-function stx)
  (syntax-parse stx
    [(_ (name params ...) body)
     (define arity (length (syntax->list #'(params ...))))
     #`(define-syntax name
         (cons #,arity #'(lambda (params ...) body)))]))

#;
(define-syntax (function-app stx)
  (syntax-parse stx
    [(_ fn args ...)
     #:do((define-values (arity function) (lookup #'fn)))
     #`(#,function args ...)]))

#;
(define-for-syntax (lookup fn)
  (define ret (syntax-local-value fn))
  (values (car ret) (cdr ret)))

#;
(define-function (s a b)
                 (+ a b))

#;
(lookup s)

#;
(function-app s 1 12)

#;
(define-for-syntax (lookup fn)
  (define ret (syntax-local-value fn))
  (values (car ret) (cdr ret)))

#;
(define-syntax (define-function stx)
  (syntax-parse stx
    [(_ (name args ...) body)
     (define arity (length (syntax->list #'(args ...))))
     #`(define-syntax name
         (cons #,arity #'(lambda (args ...) body)))]))

#;
(define-syntax (function-app stx)
  (syntax-parse stx
    [(_ fn args ...)
     #:do((define-values (arity function) (lookup #'fn)))
     #`(#,function args ...)]))

#;
(define-function (s x y z)
                 (+ x y z))

#;
(function-app s 1 2 3)

#;
(define-for-syntax (lookup fn)
  (define ret (syntax-local-value fn))
  (values (car ret) (cdr ret)))

#;
(define-syntax (define-function stx)
  (syntax-parse stx
    [(_ (name args ...) body)
     (define arity (length (syntax->list #'(args ...))))
     #`(define-syntax name
         (cons #,arity (lambda (args ...) body)))]))

#;
(define-syntax (function-app stx)
  (syntax-parse stx
    [(_ fn args ...)
     #:do((define-values (arity function) (lookup #'fn)))
     #`(#,function args ...)]))

#;
(define-function (s x y z)
                 (+ x y z))

#;
(function-app s 1 2 12)
