#lang racket
; defines constructor of a rational number as a pair of numbers
; defines selectors of numerator and denominator
; creates prosedures that sum and multiply rational numbers

(define (make-rat a b)
    (let ((c (gcd a b)))
         (cons (/ a c) (/ b c))))

(define (numer x)
    (car x))

(define (denom x)
    (cdr x))

(define (+rat x y)
    (make-rat
        (+ (* (numer x) (denom y)) 
           (* (numer y) (denom x)))
        (* (denom x) (denom y))))
        
(define (*rat x y)
    (make-rat
        (* (numer x) (numer y))
        (* (denom x) (denom y))))

(define n1 (make-rat 1 2))
(define n2 (make-rat 1 4))

(display (+rat n1 n2))
(newline)
(display (*rat n1 n2))
(newline)


;;; define rational object operators using general arithmetic

;(define (+rat x y)
;    (make-rat
;        (add (mul (numer x) (denom y)) 
;           (mul (numer y) (denom x)))
;        (mul (denom x) (denom y))))

