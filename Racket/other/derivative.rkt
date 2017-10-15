#lang racket

;;; Take a derivative of an algebraic expression
;;; Supports only very basic derivation rules

(require rackunit)

;; Take a derivativ of exp with respect to var
(define (deriv exp var)
  (cond((constant? exp var) 0)
       ((same-var? exp var) 1)
       ((sum? exp)
        (make-sum (deriv (cadr exp) var)
                  (deriv (caddr exp) var)))
       ((product? exp)
        (make-sum
         (make-product (cadr exp)
                       (deriv (caddr exp) var))
         (make-product (deriv (cadr exp) var)
                       (caddr exp))))))

;; Predicates

(define (constant? exp var)
  (and (atom? exp) (not (eq? exp var))))

(define (same-var? exp var)
  (and (atom? exp) (eq? exp var)))

(define (sum? exp)
  (and (not (atom? exp)) (eq? (car exp) '+)))

(define (product? exp)
  (and (not (atom? exp)) (eq? (car exp) '*)))

(define atom? (or/c number? symbol? boolean? string?))

;; Return siplified formula of sum of a1 and a2
(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2))
         (+ a1 a2))
        ((and (number? a1) (= a1 0))
         a2)
        ((and (number? a2) (= a2 0))
         a1)
        ((and (not (number? a1)) (eq? a1 a2))
         (list '* a1 2))
        (else (list '+ a1 a2))))

;; Return simplified formula of product of m1 and m2
(define (make-product m1 m2)
  (cond ((and (number? m1) (number? m2))
         (* m1 m2))
        ((and (number? m1) (= m1 1))
         m2)
        ((and (number? m2) (= m2 1))
         m1)
        ((or (and (number? m1) (= m1 0))
             (and (number? m2) (= m2 0)))
         0)
        ((and (not (number? m1)) (eq? m1 m2))
         (list 'expt m1 2))
        (else (list '* m1 m2))))


;; Tests

; define function foo = a*x^2 + b*x + c
(define foo '(+ (* a (* x x)) (+ (* b x) c)))

(check-equal? (deriv foo 'x) '(+ (* a (* x 2)) b))
(check-equal? (deriv foo 'a) '(* x x))
(check-equal? (deriv foo 'b) 'x)
(check-equal? (deriv foo 'c) 1)

