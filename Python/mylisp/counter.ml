
(define counter (lambda (n) ((define x 0) (lambda () ((if (< x n) ((set! x (+ x 1)) x) null))))))

(define counter
    (lambda (n) (
        (define x 0)
        (lambda () (
            (if (< x n) (
                (set! x (+ x 1)) x)
                null))))))

(define a (counter 10))

