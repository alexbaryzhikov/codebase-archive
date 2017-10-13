;#lang racket

; ---------------------------------------
; Queue
; ---------------------------------------

(define (make-queue)
  (let ((front-ptr '()) (rear-ptr '()))
    (define (empty-q?) (null? front-ptr))
    (define (front-q)
      (if (empty-q?)
          (error "FRONT called with an empty queue")
          (mcar front-ptr)))
    (define (insert-q! item)
      (let ((new-pair (mcons item '())))
        (cond ((empty-q?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-mcdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)))))
    (define (delete-q!)
      (cond ((empty-q?)
             (error "DELETE called with an empty queue"))
            (else (set! front-ptr (mcdr front-ptr)))))
    (define (print-q)
      (cond ((empty-q?) (display "()"))
            (else
             (display (format "( ~a" (mcar front-ptr)))
             (display-list (mcdr front-ptr)))))
    (define (display-list l)
      (cond ((null? l) (display " )"))
            (else (display (format " . ~a" (mcar l)))
                  (display-list (mcdr l)))))
    (define (dispatch m)
      (cond ((eq? m 'empty?) (empty-q?))
            ((eq? m 'front) (front-q))
            ((eq? m 'insert!) insert-q!)
            ((eq? m 'delete!) (delete-q!))
            ((eq? m 'print) (print-q))
            (else (error "undefined operation -- QUEUE" m))))
    dispatch))

(define (empty-queue? queue)
  (queue 'empty?))

(define (front-queue queue)
  (queue 'front))

(define (insert-queue! queue item)
  ((queue 'insert!) item))

(define (delete-queue! queue)
  (queue 'delete!))

(define (print-queue queue)
  (queue 'print))

;;; Test

;(define q1 (make-queue))
;(insert-queue! q1 'a)
;(insert-queue! q1 'b)
;(insert-queue! q1 'c)
;(print-queue q1)
