#lang racket

; integers

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (add1 n))))

; prime numbers

(define (divisible? x y)
  (= (remainder x y) 0))

(define (next-prime s)
  (stream-filter (λ (x) (not (divisible? x (stream-first s)))) (stream-rest s)))

(define (sieve s)
  (stream-cons (stream-first s)
               (sieve (next-prime s))))

(define primes (sieve (integers-starting-from 2)))