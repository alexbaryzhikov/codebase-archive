#lang racket

(define make-counter
  (lambda(n)
    (lambda()
      (set! n (add1 n))
      n)))

(define c1 (make-counter 0))
(define c2 (make-counter 10))

; Actions and Identity
;
; We say that an action, A, had an effect
; on an object, X, (or equivalently, that
; X was changed by A) if some property, P,
; which was true of X before A became
; false of X after A.
;
; We say that two objects, X and Y, are
; the same if any action which has an
; effect on X has the same effect on Y.