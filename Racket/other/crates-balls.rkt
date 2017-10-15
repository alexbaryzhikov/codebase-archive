#lang racket

;; Enumerate all possible crates fillings
(define (enum-crates num vol)
  (define arity (add1 vol)) ; Digits per rank
  (define lists-total (expt arity num)) ; Last number to check
  ; Make crates that represent n as num'ary number
  (define (make-crates n)
    (define (get-digit rank)
      (quotient (remainder n (expt arity (add1 rank)))
                (expt arity rank)))
    (build-list num get-digit))
  ; Enumerate fillings
  (build-list lists-total make-crates))

;; Filter out combinations with wrong number of balls
(define (get-crates crates balls)
  (define (number-balls-good? crate)
    (= (foldl + 0 crate) balls))
  (filter number-balls-good?
          (enum-crates crates balls)))
