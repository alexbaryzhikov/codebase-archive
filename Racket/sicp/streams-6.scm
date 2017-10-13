#lang racket

(define (add-streams s1 s2)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (stream-cons
          (+ (stream-first s1) (stream-first s2))
          (add-streams (stream-rest s1) (stream-rest s2))))))

(define (scale-stream c s)
  (stream-map (lambda (x) (* x c)) s))


(define ones (stream-cons 1 ones))

(define integers 
  (stream-cons 1
               (add-streams integers ones)))

(define fibs
  (stream-cons 0
               (stream-cons 1
                            (add-streams fibs (stream-rest fibs)))))

(define (integral delayed-s
                  initial-value
                  dt)
  (define int
    (stream-cons
     initial-value
     (let ((s (force delayed-s)))
       (add-streams (scale-stream dt s)
                    int))))
  int)

(define (make-deposit-account
         balance deposit-stream)
  (stream-cons
   balance
   (make-deposit-account
    (+ balance (stream-first deposit-stream))
    (stream-rest deposit-stream))))

