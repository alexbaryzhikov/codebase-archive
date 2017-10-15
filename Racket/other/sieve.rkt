#lang racket

;; Integers stream
(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (add1 n))))

;; x is divisible by y without remainder
(define (divisible? x y)
  (= (remainder x y) 0))

(define (next-prime s)
  (stream-filter (λ (x) (not (divisible? x (stream-first s))))
                 (stream-rest s)))

(define (sieve s)
  (stream-cons (stream-first s)
               (sieve (next-prime s))))

;; Primes stream
(define primes
  (sieve (integers-starting-from 2)))

; Output first 50 primes
(for ([i (in-range 50)])
  (printf "~a " (stream-ref primes i)))
