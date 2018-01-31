#lang racket

(provide (all-defined-out))

(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)
(struct bool (b) #:transparent)
(struct eq-num (e1 e2) #:transparent)
(struct if-then-else (e1 e2 e3) #:transparent)

(define (eval-exp e)
  (cond [(const? e) e]
        [(bool? e) e]

        [(negate? e)
         (let ([v (eval-exp (negate-e e))])
           (if (const? v)
             (const (- (const-int v)))
             ((error "negate applied to non-number"))))]

        [(add? e)
         (let ([v1 (eval-exp (add-e1 e))]
               [v2 (eval-exp (add-e2 e))])
           (cond [(not (const? v1)) (error "non-number on left hand side of add expression")]
                 [(not (const? v2)) (error "non-number on right hand side of add expression")]
                 [#t (const (+ (const-int v1) (const-int v2)))]))]

        [(multiply? e)
         (let ([v1 (eval-exp (multiply-e1 e))]
               [v2 (eval-exp (multiply-e2 e))])
           (cond [(not (const? v1)) (error "non-number on left hand side of multiply expression")]
                 [(not (const? v2)) (error "non-number on right hand side of multiply expression")]
                 [#t (const (* (const-int v1) (const-int v2)))]))]

        [(eq-num? e)
         (let ([v1 (eval-exp (eq-num-e1 e))]
               [v2 (eval-exp (eq-num-e2 e))])
           (if (and (const? v1) (const? v2))
             (bool (= v1 v2))
             (error "non-number applied to eq-num expression")))]

        [(if-then-else? e)
         (let ([check (eval-exp (if-then-else-e1 e))])
           (cond [(and (bool? check) (bool-b check)) (eval-exp (if-then-else-e2 e))]
                 [(bool? check) (eval-exp (if-then-else-e3 e))]
                 [#t (error "non-bool in if-then-else condition")]))]))
