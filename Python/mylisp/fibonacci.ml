(define fib (lambda () ((define x 0) (define y 1) (define t null) (lambda () ((set! t (+ x y)) (set! x y) (set! y t) x)))))

(define fib
    (lambda () (
        (define x 0)
        (define y 1)
        (define t null)
        (lambda () (
            (set! t (+ x y))
            (set! x y)
            (set! y t)
            x)))))

(define a (fib))
