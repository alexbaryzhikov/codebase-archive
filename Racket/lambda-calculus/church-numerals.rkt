#lang racket

; ===========================================================
; Church numerals
; ===========================================================

; initial f and x
(define i-f (λ (x) (add1 x)))
(define i-x 0)

; 0
((λ (f x) x)
 i-f
 i-x)

; 1
((λ (f x) (f x))
 i-f
 i-x)

; 2
((λ (f x) (f (f x)))
 i-f
 i-x)

; 3
((λ (f x) (f (f (f x))))
 i-f
 i-x)

; n - applies f to x recursively n times
; loop n f x - f is applied n times on x 
(define (loop n)
  (λ (f x)
    ((λ (n r) (r r n f x))
     n
     (λ (r n f x)
       (if (= n 0)
           x
           (r r (sub1 n) f (f x)))))))

((λ (n f x) ((loop n) f x))
 4
 i-f
 i-x)
  
; SUCC - successor function, takes n and returns n+1
((λ (n f x) (f ((loop n) f x)))
 4
 i-f
 i-x)

(define (SUCC n)
  ((λ (f x) (f ((loop n) f x)))
   i-f
   i-x))
  
; PLUS - takes m-th composition of f and composes with n-th composition of f
((λ (m n f x) ((loop m) f ((loop n) f x)))
 2
 4
 i-f
 i-x)

(define (PLUS m)
  (λ (n) ((loop m) SUCC n)))

; MULT
((λ (m n) ((loop m) (PLUS n) 0))
 2
 5)

(define (MULT m)
  (λ (n) ((loop m) (PLUS n) 0)))

; POW
(define (POW n)
  (λ (e) ((loop e) (MULT n) 1)))


