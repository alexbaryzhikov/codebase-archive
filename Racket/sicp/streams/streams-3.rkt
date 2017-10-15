#lang racket

(require racket/include)
(include "streams.rkt")

(define (queens size)
  (define (fill-cols k)
    (if
     (= k 0)
     (singleton empty-board)
     (collect
      (adjoin-position try-row
                       k
                       rest-queens)
      ((rest-queens (fill-cols (sub1 k)))
       (try-row (stream-enumerate-interval 1 size)))
      (safe? try-row k rest-queens))))
  (fill-cols size))


