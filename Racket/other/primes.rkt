#lang racket

(require racket/generator)

; 'Primeness' test
(define (prime? x)
  (define (iter n)
    (if (= n x) #t (if (= (remainder x n) 0) #f (iter (add1 n)))))
  (cond
    ((< x 2) (error (format "Incorrect argument: ~a -- PRIME?" x)))
    (else    (if (< x 3) #t (iter 2)))))

; Prime numbers stream
(define primes
  (stream-filter prime? (in-range 2 10000000)))

; Prime numbers generator
(define (primes-gen)
  (generator () (for ([x primes]) (yield x))))

; Output first 50 prime numbers
(define new-primes (primes-gen))
(for ([i (in-range 49)]) (printf "~a " (new-primes)))
(printf "~a\n" (new-primes))
