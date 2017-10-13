#lang racket

; integers

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (add1 n))))

(define integers (integers-starting-from 1))


; fibonacci numbers

(define (fib-gen a b)
  (stream-cons a (fib-gen b (+ a b))))

(define fibs (fib-gen 0 1))


; prime numbers

(define (divisible? x y)
  (= (remainder x y) 0))

(define (next-prime a)
  (stream-filter (lambda (x) (not (divisible? x (stream-first a)))) (stream-rest a)))

(define (sieve stream)
  (stream-cons (stream-first stream)
               (sieve (next-prime stream))))

(define primes (sieve (integers-starting-from 2)))



