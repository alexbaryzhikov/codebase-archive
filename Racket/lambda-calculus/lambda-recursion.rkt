#lang racket

; ===========================================================
; λ recursion
; ===========================================================

; explicitly re-write each recursive call as self-application

((λ (r x) (r r x))
 (λ (r x)
   (if (= x 0)
       1
       (* x (r r (sub1 x)))))
 4)

; generic solution without re-writes

; lazy Y combinator
;(define Y
;  (λ (g)
;    ((λ (x) (g (x x)))
;     (λ (x) (g (x x))))))

; strict Y combinator
(define Y
  (λ (g)
    ((λ (f) (g (λ (x) ((f f) x))))
     (λ (f) (g (λ (x) ((f f) x)))))))

(define FACT
  (Y
   (λ (r)
     (λ (n)
       (if (= n 0)
           1
           (* n (r (sub1 n))))))))

