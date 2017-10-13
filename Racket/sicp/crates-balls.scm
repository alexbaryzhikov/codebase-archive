#lang racket

(define (enum-crates num vol)
  (define vol1 (add1 vol))
  (define lists-total (expt vol1 num))
  (define (make-crate n)
    (build-list num (lambda (x) (quotient (remainder n (expt vol1 (add1 x))) (expt vol1 x)))))
  (build-list lists-total (lambda (x) (make-crate x))))

(define (get-crates crates balls)
  (define (number-balls-good? crate)
    (= (foldl + 0 crate) balls))
  (filter number-balls-good? (enum-crates crates balls)))

