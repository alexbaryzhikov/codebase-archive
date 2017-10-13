#lang racket

(define (a) (a))

(define (test x y)
  (if (= x 0)
      0
      y))
