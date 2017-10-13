#lang racket
;
; procedure sqroot takes the square root of x by iterating averaging guess and x/guess
;   until guessed value reaches the desired accuracy
;
; average of two values
(define (average n1 n2)
	(/ (+ n1 n2) 2)
)
; absolute value of n
(define (absol n)
	(if (< n 0) (- n) n)
)
; square of n
(define (square n)
	(* n n)
)
; iteration of guessing a square root of x
(define (sqroot-iter guess x)
	(if (good-enough guess x)
		guess
		(sqroot-iter (improve guess x) x)
	)
)
; guess improvement
(define (improve guess x)
	(average guess (/ x guess))
)
; what's good enough
(define (good-enough guess x)
	(< (absol (- x (square guess))) 0.001)
)
; take square root (finally)
(define (sqroot x)
	; shoot 1 as the initial guess
	(sqroot-iter 1.0 x)
)
; test the whole thing
(display (sqroot 5))
(newline)
