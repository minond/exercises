#lang br/quicklang

(provide * / + -
         (rename-out [stackerizer-exp #%module-begin]))

(define-macro (stackerizer-exp EXPR)
  #'(#%module-begin
     (for-each displayln (reverse (flatten EXPR)))))

(define-macro (define-ops OP ...)
  #'(begin
      (define-macro-cases OP
        [(OP H) #'H]
        [(OP H T (... ...)) #'(list 'OP H (OP T (... ...)))])
      ...))

(define-ops * / + -)
