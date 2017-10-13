#lang racket
; define a "pair" data structure

; constructor
(define pair
    (lambda(a b)
        (lambda(x)
            (cond ((= x 1) a)
                  ((= x 2) b)))))

; selectors
(define (fst x) (x 1))

(define (sec x) (x 2))

; test

(define p (pair 24 3))

(display ((pair 24 3) 1))
(newline)
(display (sec p))
(newline)
