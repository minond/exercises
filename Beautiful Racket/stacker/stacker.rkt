#lang br/quicklang

(provide * / + -
         handle
         read-syntax
         (rename-out [stacker-module-begin #%module-begin]))

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '(handle ~a) src-lines))
  (define module-datum `(module stacker-mod "stacker.rkt"
                          ,@src-datums))
  (datum->syntax #f module-datum))

(define-macro (stacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     HANDLE-EXPR ...
     (display (car stack))))

(define stack empty)

(define (push-stack! x)
  (set! stack (cons x stack)))

(define (pop-stack!)
  (define x (car stack))
  (set! stack (cdr stack))
  x)

(define (handle [arg #f])
  (cond [(number? arg) (push-stack! arg)]
        [arg (push-stack! (arg (pop-stack!) (pop-stack!)))]))
