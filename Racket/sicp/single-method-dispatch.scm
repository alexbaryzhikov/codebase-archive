#lang racket
(define newObject
  (λ (value)
    (λ (action v)
      (cond ((eq? action 'get) value)
            ((eq? action 'set) (set! value v))
            (else (displayln "invalid action"))))))

(define d (newObject 0))
