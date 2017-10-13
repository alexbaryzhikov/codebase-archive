#lang lazy

;;; Can F be computed?
; F
(define EXPT
  (λ (r)
    (λ (x n)
      (cond ((= n 0) 1)
            (else
             (* x (r x (sub1 n))))))))
  
; x is a fixed point of f if f(x) = x

; We need to develop means to get fixed point of F

; The simplest infinite loop:
; ((lambda (x) (x x)) (lambda (x) (x x)))

; Curry's Paradoxical Combinator of Y
(define Y
  (λ (f)
    ((λ (x) (f (x x)))
     (λ (x) (f (x x))))))

; (Y F) = (F (Y F))
; Y produces the fixed point of F

((Y EXPT) 2 3)