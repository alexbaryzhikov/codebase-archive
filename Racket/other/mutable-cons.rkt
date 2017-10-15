#lang racket

(require rackunit)

;; Mutable pair
(define (mcons a b)
  (λ (m)
    (m a
       b
       (λ (val) (set! a val))
       (λ (val) (set! b val)))))

(define (mcar p)
  (p (lambda(a b sa sb) a)))

(define (mcdr p)
  (p (lambda(a b sa sb) b)))

(define (set-car! p val)
  (p (lambda(a b sa sb) (sa val))))

(define (set-cdr! p val)
  (p (lambda(a b sa sb) (sb val))))

;; Tests
(define a (mcons 5 10))
(check-equal? (mcar a) 5)
(check-equal? (mcdr a) 10)
(set-car! a 99)
(set-cdr! a 7)
(check-equal? (mcar a) 99)
(check-equal? (mcdr a) 7)
