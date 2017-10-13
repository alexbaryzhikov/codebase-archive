#lang racket
(define (abs a)
    (if (< a 0) (- a) a))

(define (average a b)
	(/ (+ a b) 2))

(define (fixed-point f start)
    (define (close-enough? a b)
        (< (abs (- a b)) 0.0001))
    (define (iter old new)
        (if (close-enough? old new)
            new
            (iter new (f new))))
    (iter start (f start)))

(define average-damp
    (lambda(f)
	    (lambda(i) (average (f i) i))))

(define (sqrt x)
	(fixed-point 
		(average-damp (lambda(i) (/ x i)))
		1))

(define return (sqrt 5))

(display return)
(newline)
