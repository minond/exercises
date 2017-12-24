(define debug
  (lambda (x)
    (begin
      (newline)
      (newline)
      (display x)
      (display " => ")
      (display (eval x user-initial-environment)))))
