#lang racket
(define nil '())

(define 1-to-4 (list 1 2 3 4))

(define (map p l)
    (if (null? l)
        nil
        (cons (p (car l))
              (map p (cdr l)))))

(define (scale-list x l)
    (map (lambda(item) (* item x))
         l))

(display (scale-list 10 1-to-4))
(newline)
(display (map square 1-to-4))
(newline)

(define (for-each proc list)
    (cond ((null? list) "done")
          (else (proc (car list))
                (for-each proc
                          (cdr list)))))

(for-each display 1-to-4)
(newline)
