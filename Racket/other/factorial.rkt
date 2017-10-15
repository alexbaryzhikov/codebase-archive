#lang racket

(require rackunit)

;; Factorial
(define/match (fact x)
  [(0) 1]
  [(n) (* n (fact (sub1 n)))])

;; Test
(check-equal? (fact 5) 120)
