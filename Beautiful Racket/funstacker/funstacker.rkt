#lang br/quicklang

(provide * / + -
         handle-args
         read-syntax
         (rename-out [funstacker-module-begin #%module-begin]))

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '~a src-lines))
  (define module-datum `(module funstacker-mod "funstacker.rkt"
                          (handle-args ,@src-datums)))
  (datum->syntax #f module-datum))

(define-macro (funstacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     (display HANDLE-EXPR ...)))

(define (handle-args . args)
  (for/fold [(stack '())]
            [(arg args) #:unless (void? arg)]
    (cond [(number? arg) (cons arg stack)]
          [else (cons (arg (car stack) (car (cdr stack)))
                      (cdr (cdr stack)))])))
