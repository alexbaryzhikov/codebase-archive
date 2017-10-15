#lang racket
;
; procedure fn takes 3 arguments and returns the sum of the suqares of 2 largest
;
(define (fn n1 n2 n3)
    (cond   ((and (< n1 n2) (< n1 n3)) (+ (* n2 n2) (* n3 n3)))
            ((and (< n2 n1) (< n2 n3)) (+ (* n1 n1) (* n3 n3)))
            (else (+ (* n1 n1) (* n2 n2)))))
; test the procedure fn
(display (fn 1 2 3))
(newline)
