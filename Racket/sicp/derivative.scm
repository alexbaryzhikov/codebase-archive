; derivatve.rkt
; the program takes a derivative from an algebraic expression

#lang racket

; atom? is true when exp is not a list
(define atom? (or/c number? symbol? boolean? string?))

(define (deriv exp var)
  (cond((constant? exp var) 0)
       ((same-var? exp var) 1)
       ((sum? exp)
        (make-sum (deriv (a1 exp) var)
                  (deriv (a2 exp) var)))
       ((product? exp)
        (make-sum
           (make-product (m1 exp)
                         (deriv (m2 exp) var))
           (make-product (deriv (m1 exp) var)
                         (m2 exp))))))


; eq? is true when exp is equal to var
(define (constant? exp var)
  (and (atom? exp)
       (not (eq? exp var))))

(define (same-var? exp var)
  (and (atom? exp)
       (eq? exp var)))

(define (sum? exp)
  (and (not (atom? exp))
       (eq? (car exp) '+)))

(define (make-sum a1 a2)
  (cond ((and (number? a1)
              (number? a2))
         (+ a1 a2))
        ((and (number? a1) (= a1 0))
         a2)
        ((and (number? a2) (= a2 0))
         a1)
        ((and (not (number? a1))
              (eq? a1 a2))
         (list '* a1 2))
        (else (list '+ a1 a2))))
(define a1 cadr)
(define a2 caddr)

(define (product? exp)
  (and (not (atom? exp))
       (eq? (car exp) '*)))

(define (make-product m1 m2)
  (cond ((and (number? m1)
              (number? m2))
         (* m1 m2))
        ((and (number? m1) (= m1 1))
         m2)
        ((and (number? m2) (= m2 1))
         m1)
        ((or (and (number? m1) (= m1 0))
             (and (number? m2) (= m2 0)))
         0)
        ((and (not (number? m1))
              (eq? m1 m2))
         (list 'expt m1 2))
        (else (list '* m1 m2))))
(define m1 cadr)
(define m2 caddr)

; define function foo = a*x^2 + b*x + c
(define foo
  '(+ (* a (* x x))
      (+ (* b x)
         c)))

(displayln (deriv foo 'x))
(displayln (deriv foo 'a))
(displayln (deriv foo 'b))
(displayln (deriv foo 'c))

