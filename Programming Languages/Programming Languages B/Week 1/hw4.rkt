#lang racket

(provide (all-defined-out))

#| Write a function sequence that takes 3 arguments low, high, and stride, all
   assumed to be numbers. Further assume stride is positive. sequence produces
   a list of numbers from low to high (including low and possibly high)
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

#| Write a function string-append-map that takes a list of strings xs and a
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

#| Write a function list-nth-mod that takes a list xs and a number n. If the
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
