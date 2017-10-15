#lang racket

; -------------------------------------------------
; Streams
; -------------------------------------------------

(define (cons-stream x y)
  (cons x (delay y)))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define (delay x)
  (memo-proc (lambda() x)))

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? #t)
            result)
          result))))

(define (force x)
  (x))

(define the-empty-stream null)
(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (sub1 n))))

;;; MAP, FILTER, ACCUMULATE are conventional interfaces!

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))

(define (stream-filter pred s)
  (cond
    ((stream-null? s) the-empty-stream)
    ((pred (stream-car s))
     (cons-stream (stream-car s)
                  (stream-filter pred
                                 (stream-cdr s))))
    (else (stream-filter pred (stream-cdr s)))))

(define (accumulate combiner init-val s)
  (if (stream-null? s)
      init-val
      (combiner (stream-car s)
                (accumulate combiner
                            init-val
                            (stream-cdr s)))))

;;; other procedures

(define (stream-for-each proc s)
  (if (stream-null? s)
      (void)
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (displayln x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (add1 low) high))))

(define (stream-enumerate-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree
                   the-empty-stream)
      (stream-append
       (stream-enumerate-tree
        (left-branch tree))
       (stream-enumerate-tree
        (right-branch tree)))))

(define (leaf-node? x)
  (not (pair? x)))

(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (flatmap f s)
  (flatten (map f s)))

(define (flatten st-of-st)
  (accumulate stream-append
              the-empty-stream
              st-of-st))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))





