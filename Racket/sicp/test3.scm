#lang racket

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else
         (error "invalid signal" s))))

(define (logical-and s1 s2)
  (if (and (or (= s1 1) (= s1 0))
           (or (= s2 1) (= s2 0)))
      (if (and (= s1 1) (= s2 1))
          1
          0)
      (error (format "invalid signal ~a or ~a" s1 s2))))

(define (logical-or s1 s2)
  (if (and (or (= s1 1) (= s1 0))
           (or (= s2 1) (= s2 0)))
      (if (or (= s1 1) (= s2 1))
          1
          0)
      (error (format "invalid signal ~a or ~a" s1 s2))))
