#lang racket

(define (average a b)
  (+ a (/ (- b a) 2)))

;; Return a fixed point of f
(define (fixed-point f start)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.0001))
  (define (iter old new)
    (if (close-enough? old new)
        new
        (iter new (f new))))
  (iter start (f start)))

;; Decorator: wrapper = average(x, f(x))
(define (average-damp f)
  (λ (x) (average x (f x))))

;; Compute square root as fixed point
(define (sqrt x)
  (fixed-point (average-damp (λ (i) (/ x i))) 1))

(displayln (sqrt 7))
