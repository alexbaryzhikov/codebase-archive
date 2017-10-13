#lang racket
(define (sum term a next b)
    (define (iter j ans)
        (if (> j b)
            ans
            (iter (next j)
                    (+ (term j) ans))))
    (iter a 0))
(define return (sum (lambda(i) (* i i)) 2 (lambda(i) (+ i 1)) 4))

(display "Sum = ")
(display return)
(newline)
