#lang racket

; Cesaro's method for estimating Pi:
;   Prob(gcd(n1,n2)=1) = 6/(Pi*Pi)

(define (estimate-pi n)
  (sqrt (/ 6 (monte-carlo n cesaro))))

(define (cesaro)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter remaining passed)
    (cond ((= remaining 0)
           (/ passed trials))
          ((experiment)
           (iter (sub1 remaining) (add1 passed)))
          (else
           (iter (sub1 remaining) passed))))
  (iter trials 0))

(define (rand)
  (random 4294967087))

; run the estimation
(estimate-pi 100)
