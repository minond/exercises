#lang racket

(require (for-syntax syntax/parse syntax/stx))

(provide + if lambda
         (rename-out [typechecking-mb #%module-begin]))

(define-syntax (typechecking-mb stx)
  (syntax-parse stx
    [(_ e ...)
     #:do[(for ([e (syntax->list #'(e ...))])
            (printf "~e : ~a\n"
                    (syntax->datum e)
                    (syntax->datum (compute e (mk-empty-env)))))]
     #`(#%module-begin (void))]))

(begin-for-syntax
  ; mk-empty-env : -> TyEnv
  ; Returns a new type environment with no bindings.
  (define (mk-empty-env)
    '())

  ; add-to-env : TyEnv IdStx TyStx -> TyEnv
  ; Returns a new type environment that extends the given env with the given binding.
  (define (add-to-env ctx id typ)
    (cons `(,id . ,typ) ctx))

  ; lookup-env : TyEnv IdStx -> TyStx or #f
  ; Looks up the given id in the given env and returns corresponding type. Returns false if the id is not in the env.
  (define (lookup-env ctx id)
    (cond ((null? ctx) #f)
          ((eqv? (car (car ctx)) id) (cdr (car ctx)))
          (#t (lookup-env (cdr ctx) id))))

  ; check : ExprStx TyStx -> Bool
  ; checks that the given term has the given type
  (define (check ctx e t-expected)
    (define t (compute e ctx))
    (or (type=? t t-expected)
        (raise-syntax-error
          'check
          (format "error while checking term ~a: expected ~a; got ~a"
                  (syntax->datum e)
                  (syntax->datum t-expected)
                  (syntax->datum t)))))

  ; type=? : TyStx TyStx -> Bool
  ; type equality here is is stx equality
  (define (type=? t1 t2)
    (or (and (identifier? t1) (identifier? t2) (free-identifier=? t1 t2))
        (and (stx-pair? t1) (stx-pair? t2)
             (= (length (syntax->list t1))
                (length (syntax->list t2)))
             (andmap type=? (syntax->list t1) (syntax->list t2)))))

  (define (compute stx ctx)
    (syntax-parse stx
      #:datum-literals(+ if λ)
      [:number #'Number]
      [:boolean #'Boolean]
      [:string #'String]

      [v:id
        #:do((define t (lookup-env ctx (syntax->datum #'v))))
        #:when t
        t]

      [(+ a:expr b:expr)
       (if (and (check ctx #'a #'Number) (check ctx #'b #'Number))
         #'Number
         (raise-syntax-error 'typecheck "bad call to +"))]
      [(if e1 e2 e3)
       #:when (check ctx #'e1 #'Boolean)
       #:with t2 (compute #'e2 ctx)
       #:when (check ctx #'e3 #'t2)
       #'t2]

      [(λ ([n:id : typ:id] ...) body)
       #:do((define scope (foldl (lambda (id typ ctx) (add-to-env ctx
                                                                  (syntax->datum id)
                                                                  typ))
                                 ctx
                                 (syntax->list #'(n ...))
                                 (syntax->list #'(typ ...)))))

       #:with ret (compute #'body scope)

       #`(-> typ ... ret)]

      [(fn:expr param:expr ...)
       #:with (-> intyp ... r) (compute #'fn ctx)
       #:with argtyp (map (lambda (p) (compute p ctx)) (syntax->list #'(param ...)))
       #:when (type=? #'(intyp ...) #'argtyp)
       #'r]

      [e (raise-syntax-error 'typecheck (format "unknown: ~a" (syntax->datum #'e)) stx)])))
