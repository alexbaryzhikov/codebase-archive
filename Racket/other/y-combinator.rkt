#lang racket

(require rackunit)

; A fixed point of f is x such that f(x) = x.
; We need to develop means to get a fixed point of f.

;; Curry's Paradoxical Combinator of Y
(define (Y f)
  ((λ (r) (f (delay r r)))
   (λ (r) (f (delay r r)))))

; (Y F) = (F (F (... (Y F))))
; Y produces the fixed point of F

;; Recursive exponentiation via Y-combinator
(define (pow r)
  (λ (x n)
    (define next-call ((force r) (force r)))
    (cond ((= n 0) 1)
          (else (* x (next-call x (sub1 n)))))))

;; Base to the power of exponent
(define (power base exponent)
  ((Y pow) base exponent))

;; Tests
(check-equal? (power 0 99) 0)
(check-equal? (power 1 99) 1)
(check-equal? (power 2 4) 16)
(check-equal? (power 3 3) 27)
(check-equal? (power 10 5) 100000)
