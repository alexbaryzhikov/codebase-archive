#lang racket

; Sum of terms
; 'term' is a function with one bound variable, which
; is iterated from 'a' to 'b' via 'next' function.
(define (sum a b next term)
  (let loop ([j a] [ans 0])
    (if (< j b)
        (loop (next j) (+ (term j) ans))
        ans )))

; Sum of squares
(define (sumsquares a b)
  (sum a b (λ(i) (add1 i)) (λ(i) (* i i))))

; Test
(printf "Sum of squares from 2 to 4 = ~a\n" (sumsquares 2 4))
