#lang racket
(define fn1
    (lambda(f)
        (lambda(x) (* (+ x (f x)) 2))))
    
(define fn2
    (lambda(x) (+ x 1)))

(define fn3
    (lambda(x) ((fn1 fn2) x)))

(display (fn2 3))
(newline)
(display (fn3 3))
(newline)
