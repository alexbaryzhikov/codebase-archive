#lang racket

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

(define (make-counter start)
  (define n (sub1 start))
  (λ ()
    (set! n (add1 n)) ; side effect
    n))

(define c1 (make-counter 0))
(printf "~a ~a ~a\n" (c1) (c1) (c1))
