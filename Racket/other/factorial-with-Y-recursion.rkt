#lang racket

(require rackunit)

(define factorial-with-Y-recursion
  ((λ (g)
     ((λ (x) (x x))
      (λ (x) (g (λ (n) ((x x) n))))))
   (λ (factorial) 
     (λ (n) 
       (cond ((= n 0) 1)
             ((= n 1) 1)
             (else (* n (factorial (- n 1)))))))))

(check-equal? (factorial-with-Y-recursion 0) 1)
(check-equal? (factorial-with-Y-recursion 1) 1)
(check-equal? (factorial-with-Y-recursion 2) 2)
(check-equal? (factorial-with-Y-recursion 3) 6)
(check-equal? (factorial-with-Y-recursion 4) 24)
(check-equal? (factorial-with-Y-recursion 5) 120)
