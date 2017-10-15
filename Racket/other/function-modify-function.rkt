#lang racket

(require rackunit)

;; Decorator
(define (fn1 f)
  (λ (x) (* (+ x (f x)) 2)))
    
;; Original function
(define fn2
  (λ (x) (+ x 1)))

;; Decorated f2
(define fn3
  (λ (x) ((fn1 fn2) x)))

;; Tests
(check-equal? (fn2 3) 4)
(check-equal? (fn3 3) 14)
