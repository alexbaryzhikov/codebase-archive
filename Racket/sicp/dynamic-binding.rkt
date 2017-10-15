#lang racket

(define sum
  (lambda (term a next b)
    (cond ((> a b) 0)
          (else
           (+ (term a)
              (sum term
                   (next a)
                   next
                   b))))))

(define product
  (lambda (term a next b)
    (cond ((> a b) 0)
          (else
           (* (term a)
              (product term
                       (next a)
                       next
                       b))))))

(define sum-powers
  (lambda (a b n)
    (sum (lambda (x) (expt x n))
         a
         add1
         b)))

(define sum-powers2
  (lambda (a b n)
    (sum nth-power a add1 b)))


(define product-powers
  (lambda (a b n)
    (product (lambda (x) (expt x n))
             a
             add1
             b)))

(define product-powers2
  (lambda (a b n)
    (product nth-power a add b)))

(define nth-power
  (lambda(x)
    (expt x n)))

; In dynamic binding language n will be searche through the call stack.
; But the problem is that it will use the first n found, which can
; coinside with n of some procedure in the cass stack, but not the
; intended one. So in turn this will make modularity difficult, because
; binded variables names now may conflict
; So it's better to do like this:

(define pgen
  (lambda (n)
    (lambda (x)
      (expt x n))))

(define sum-powers2
  (lambda (a b n)
    (sum (pgen n) a add1 b)))

(define product-powers2
  (lambda (a b n)
    (product (pgen n) a add b)))


