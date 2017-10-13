#lang racket
;(require racket/include)
;(include "streams.rkt")

(define (prime? x)
  (define (iter n)
    (if (= n x)
        #t
        (if (= (remainder x n) 0)
            #f
            (iter (add1 n)))))
  (cond ((< x 2) (error (format "incorrect argument: ~a -- PRIME?" x)))
        (else
         (if (< x 3)
             #t
             (iter 2)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (add1 low) high))))

;(define a (stream-filter prime? (stream-enumerate-interval 2 100000)))
(define a (stream-filter prime? (stream-enumerate-interval 10000 10000000)))

(define i -1)
(define (next-i)
  (set! i (add1 i))
  i)

(define (propagate)
  (next-i)
  (stream-ref a i))

 (define (counter)
   (let ((j 0))
     (lambda()
       (displayln j)
       (set! j (add1 j)))))



      
    
