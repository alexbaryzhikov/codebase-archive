#lang racket

(require rackunit)

;; Passing arguments by by value
(define (unless p c a)
  (cond
    ((not p) c)
    (else a)))

;; (unless (= 1 0) 2 (/ 1 0)) is supposed to transform to
;; (cond
;;   ((not (= 1 0)) 2)
;;   (else (/ 1 0)))
;; but fails due to arguments evaluation on application

;; Passing arguments with laziness
(define (unlessd p c a)
  (cond
    ((not p) (undelay c))
    (else (undelay a))))

;; Make lazy expression
(define (make-delay func args)
  (cons 'thunk (cons func args)))

;; Evaluate lazy expression
(define (undelay v)
  (cond
    ((and (pair? v) (eq? (car v) 'thunk))
     (undelay-run (cdr v)))
    ((pair? v)
     (cons (undelay (car v)) (undelay (cdr v))))
    ((list? v)
     (map undelay v))
    (else v)))

;; Helper function for undelay
(define (undelay-run v)
  (if (and (pair? v) (eq? (car v) 'thunk))
      (undelay-run (cdr v))
      (apply (undelay (car v)) (undelay (cdr v)))))


;; Test make-delay
(define a (make-delay + '(4 2)))
(define b (make-delay * '(5 10)))
(define c (make-delay - (list b a)))
(check-equal? (undelay a) 6)
(check-equal? (undelay b) 50)
(check-equal? (undelay c) 44)

;; Test unlessd
(check-equal? (unlessd (= 1 0) 2 (make-delay / (list 1 0))) 2)
