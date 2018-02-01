;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem 1

#| 1. Warm-Up:

   (a) Write a Racket function racketlist->mupllist that takes a Racket list
       (presumably of mupl values but that will not affect your solution) and
       produces an analogous mupl list with the same elements in the same
       order.

   (b) Write a Racket function mupllist->racketlist that takes a mupl list
       (presumably of mupl values but that will not affect your solution) and
       produces an analogous Racket list (of mupl values) with the same
       elements in the same order. |#

(define (racketlist->mupllist xs)
  (if (null? xs)
    (aunit)
    (apair (car xs) (racketlist->mupllist (cdr xs)))))

(define (mupllist->racketlist es)
  (if (not (apair? es))
    null
    (cons (apair-e1 es) (mupllist->racketlist (apair-e2 es)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]

        ; All values (including closures) evaluate to themselves. For example,
        ; (eval-exp (int 17)) would return (int 17), not 17.
        [(int? e) e]

        ; An ifgreater evaluates its first two subexpressions to values v1 and
        ; v2 respectively. If both values are integers, it evaluates its third
        ; subexpression if v1 is a strictly greater integer than v2 else it
        ; evaluates its fourth subexpression.
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
             (if (> (int-num v1) (int-num v2))
               (eval-under-env (ifgreater-e3 e) env)
               (eval-under-env (ifgreater-e4 e) env))
             (error "MUPL ifgreater applied to non-number")))]

        ; Functions are lexically scoped: A function evaluates to a closure
        ; holding the function and the current environment.
        [(fun? e) (closure env e)]

        ; A call evaluates its first and second subexpressions to values. If
        ; the first is not a closure, it is an error. Else, it evaluates the
        ; closure’s function’s body in the closure’s environment extended to
        ; map the function’s name to the closure (unless the name field is #f)
        ; and the function’s argument-name (i.e., the parameter name) to the
        ; result of the second subexpression.
        [(call? e)
         (let ([fclosure (call-funexp e)]
               [farg (eval-under-env (call-actual e) env)])
           (if (not (closure? fclosure))
             (error "MUPL call applied to non-closure")
             (letrec ([fenv (closure-env fclosure)]
                      [ffun (closure-fun fclosure)]
                      [tmpenv (cons (cons (fun-formal ffun) farg) fenv)]
                      ; This cons will be '(#f . (closure ...)) whenever we get
                      ; a (fun #f ...) but I think that's ok.
                      [runenv (cons (cons (fun-nameopt ffun) fclosure) tmpenv)])
               (eval-under-env (fun-body ffun) runenv))))]

        ; An mlet expression evaluates its first expression to a value v. Then
        ; it evaluates the second expression to a value, in an environment
        ; extended to map the name in the mlet expression to v.
        [(mlet? e)
         (letrec ([val (eval-under-env (mlet-e e) env)]
                  [key (mlet-var e)]
                  [sub (cons (cons key val) env)])
           (eval-under-env (mlet-body e) sub))]

        [(apair? e) (error "unimplemented apair")]
        [(fst? e) (error "unimplemented fst")]
        [(snd? e) (error "unimplemented snd")]
        [(aunit? e) (error "unimplemented aunit")]
        [(isaunit? e) (error "unimplemented isaunit")]

        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3) "CHANGE")

(define (mlet* lstlst e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define mupl-map "CHANGE")

(define mupl-mapAddN
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
