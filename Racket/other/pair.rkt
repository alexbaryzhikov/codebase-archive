#lang racket

;;; A "pair" data structure

;; Constructor
(define (pair a b)
  (λ (f)
    (f a b)))

;; Selectors
(define (fst p) (p (λ (a b) a)))
(define (sec p) (p (λ (a b) b)))

;; Tests
(define p (pair 24 3))
(displayln (fst p))
(displayln (sec p))
