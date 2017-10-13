#lang racket
; representing vectors in the plane
(define make-vector cons)  ; constructor
(define xcor car)  ; selector
(define ycor cdr)

; representing line segments
(define (make-segment p q) (cons p q))
(define (seg-start s) (car s))
(define (seg-end s) (cdr s))

; midpoint of a segment
(define (average a b) (/ (+ a b) 2))

(define (midpoint s)
    (let ((a (seg-start s))
          (b (seg-end s)))
    (make-vector
        (average (xcor a) (xcor b))
        (average (ycor a) (ycor b)))))

; length of a segment
(define (length s)
    (let ((dx (abs (- (xcor (seg-end s))
                      (xcor (seg-start s)))))
          (dy (abs (- (ycor (seg-end s))
                      (ycor (seg-start s))))))
        (sqrt (+ (square dx)
                 (square dy)))))
