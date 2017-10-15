#lang racket

; Subject: generic arithmetic system

;;; defining generic operations

(define (add x y)
  (operate-2 'add x y))

(define (operate-2 op arg1 arg2)
  (if
   (eq? (type arg1) (type arg2))
   (let ((proc (get (type arg1) op)))
     (if (not (null? proc))
         (proc (contents arg1)
               (contents arg2))
         (error
          "Op undefined on type")))
   (error "Args not same type")))

; the same for sub mul div


;;; installing complex numbers

(define (make-complex z)
  (attach-type 'complex z))

(define (+complex z1 z2)
  (make-complex (+c z1 z2)))

(put 'complex 'add +complex)

; similarly for -complex *complex /complex


;;; installing ordinary numbers

(define (make-number n)
  (attach-type 'number n))

(define (+number x y)
  (make-number (+ x y)))

(put 'number 'add +number)

; similarly for -number *number /number



