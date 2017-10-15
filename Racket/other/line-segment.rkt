#lang racket

(define (average a b) (+ a (/ (- b a) 2)))
(define (square x) (expt x 2))

;; Represent vector in the plane
(define make-vector cons)  ; constructor
(define xcor car)  ; selector
(define ycor cdr)

;; Represent line segment
(define (make-segment p q) (cons p q))
(define (seg-start s) (car s))
(define (seg-end s) (cdr s))

;; Midpoint of a segment
(define (midpoint s)
  (let ([a (seg-start s)]
        [b (seg-end s)])
    (make-vector
     (average (xcor a) (xcor b))
     (average (ycor a) (ycor b)))))

;; Length of a segment
(define (length s)
  (let ([dx (abs (- (xcor (seg-end s))
                    (xcor (seg-start s))))]
        [dy (abs (- (ycor (seg-end s))
                    (ycor (seg-start s))))])
    (sqrt (+ (square dx)
             (square dy)))))
