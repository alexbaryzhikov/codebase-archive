#lang racket

; Functional programs
; encode mathematical truths

(define (fact n)
  (cond ((= n 1) 1)
        (else (* n (fact (- n 1))))))

; substitution works for functional programs
; (fact 3)
; (* 3 (fact 2))
; (* 3 (* 2 (fact 1)))
; (* 3 (* 2 1))
; (* 3 2)
; 6

; Methods may be distinguished by
; the choice of truths expressed

; recursive
(define (+ n m)
  (cond ((= n 0) m)
        (else (add1 (+ (sub1 n) m)))))

; iterative
(define (+ n m)
  (cond ((= n 0) m)
        (else (+ (sub1 n) (add1 m)))))


(define (fact n)
  (define (iter m i)
    (cond ((> i n) m)
          (else (iter (* i m) (add1 i)))))
  (iter 1 1))
