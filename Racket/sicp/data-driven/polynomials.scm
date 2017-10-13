#lang racket

; Subject: general arithmetic on polynomials

; polynomial prototype

(polynomial x <term-list>)

; <term-list> - ((order coeff) (order coeff) ...)
((15 1) (7 2) (0 5))

;;; installing polynomials

(define (make-polynomial var temr-list)
  (attach-type 'polynomial
               (cons var term-list)))

(define (+poly p1 p2)
  (if (same-var? (var p1) (var p2))
      (make-polynomial
       (var p1)
       (+terms (term-list p1)
               (term-list p2)))
      (error "Polys not in same var")))

(put 'polynomial 'add +poly)

(define (+terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1))
               (t2 (first-term L2)))
           (cond
             ((> (order t1) (order t2))
              (adjoin-term
               t1
               (+terms (rest-terms L1) L2)))
             ((< (order t1) (order t2))
              (adjoin-term
               t2
               (+terms L1 (rest-terms L2))))
             (else
              (adjoin-term
               (make-term (order t1)
                          (add (coef t1)
                               (coef t2)))
               (+terms (rest-terms L1)
                       (rest-terms L2)))))))))

