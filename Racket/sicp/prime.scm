#lang racket

(define (prime? x)
  (define (iter n)
    (if (= n x)
        #t
        (if (= (remainder x n) 0)
            #f
            (iter (add1 n)))))
  (cond ((< x 2) (error (format "incorrect argument: ~a -- PRIME?" x)))
        (else
         (if (< x 3)
             #t
             (iter 2)))))
