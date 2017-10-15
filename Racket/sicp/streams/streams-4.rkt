#lang racket

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

(stream-first (stream-rest (stream-filter prime?
                                          (stream-enumerate-interval 10000 1000000))))


(define a (stream-filter prime? (stream-enumerate-interval 10000 10000000)))

(define (counter)
  (let ((j -1))
    (lambda()
      (set! j (add1 j))
      (displayln (format "prime ~a" j))
      (stream-ref a j))))

(define propagate (counter))


