#lang racket

(define (square x) (expt x 2))
(define NIL '())
(define 1-to-4 (list 1 2 3 4))

;; Map p to each element of l
(define (map p l)
  (if (null? l)
      NIL
      (list* (p (car l))
             (map p (cdr l)))))

(define (scale-list x l)
  (map (λ (item) (* item x)) l))

(displayln (scale-list 10 1-to-4))
(displayln (map square 1-to-4))

;; Apply proc to every element of list
(define (for-each proc list)
  (if (not (null? list))
      (begin
        (proc (car list))
        (for-each proc (cdr list)))
      (void)))

(for-each display 1-to-4)
