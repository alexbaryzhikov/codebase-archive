#lang racket

(define (foo)
  (define (bar x) (* x 2))
  bar)

(define a (foo))

(displayln (a 3))