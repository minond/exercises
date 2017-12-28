(define debug
  (lambda (x)
    (begin
      (newline)
      (newline)
      (display x)
      (display " => ")
      (display (eval x user-initial-environment)))))

(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))
