#lang racket

(provide (all-defined-out))

#| 1. Write a function sequence that takes 3 arguments low, high, and stride,
   all assumed to be numbers. Further assume stride is positive. sequence
   produces a list of numbers from low to high (including low and possibly high)
   separated by stride and in sorted order. Sample solution: 4 lines.
   Examples:

     (println (sequence 3 11 2)) ; ’(3 5 7 9 11)
     (println (sequence 3 8 3)) ; ’(3 6)
     (println (sequence 3 2 1)) ; ’() |#
(define (sequence low high stride)
  (if (>= low high)
      '()
      (let ([next (+ low stride)])
        (cons next (sequence next high stride)))))

#| 2. Write a function string-append-map that takes a list of strings xs and a
   string suffix and returns a list of strings. Each element of the output
   should be the corresponding element of the input appended with suffix (with
   no extra space between the element and suffix). You must use Racket-library
   functions map and string-append. Sample solution: 2 lines. Test:

     (println (string-append-map
      '("Marcos" "Minond" "Estrada" "Oscar")
      "Mr. ")) |#
(define (string-append-map xs suffix)
  (map (lambda (item)
         (string-append suffix item))
       xs))

#| 3. Write a function list-nth-mod that takes a list xs and a number n. If the
   number is negative, terminate the computation with (error "list-nth-mod:
   negative number"). Else if the list is empty, terminate the computation with
   (error "list-nth-mod: empty list"). Else return the ith element of the list
   where we count from zero and i is the remainder produced when dividing n by
   the list’s length. Library functions length, remainder, car, and list-tail
   are all useful – see the Racket documentation. Sample solution is 6 lines.
   Test:

     (println (list-nth-mod '(1 2 3 4 5) 2)) |#
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([ith (remainder n (length xs))])
                 (car (list-tail xs n)))]))

#| 4. Write a function stream-for-n-steps that takes a stream s and a number n.
   It returns a list holding the first n values produced by s in order. Assume
   n is non-negative. Sample solution: 5 lines. Note: You can test your streams
   with this function instead of the graphics code. |#
(define (stream-for-n-steps s n)
  (letrec ([nth (lambda (i stream)
                  (cond [(zero? i) (car (stream))]
                        [#t (nth (- i 1) (cdr (stream)))]))]
           [next (lambda (i)
                  (cond [(= i n) null]
                        [#t (cons (nth i s) (next (+ i 1)))]))])
    (next 0)))

#| 5. Write a stream funny-number-stream that is like the stream of natural
   numbers (i.e., 1, 2, 3, ...) except numbers divisble by 5 are negated
   (i.e., 1, 2, 3, 4, -5, 6, 7, 8, 9, -10, 11, ...). Remember a stream is a
   thunk that when called produces a pair. Here the car of the pair will be a
   number and the cdr will be another stream. |#
(define funny-number-stream
  (letrec ([f (lambda (n)
                (cons (cond [(zero? (modulo n 5)) (- n)]
                            [#t n]) (lambda () (f (+ n 1)))))])
    (lambda () (f 1))))
