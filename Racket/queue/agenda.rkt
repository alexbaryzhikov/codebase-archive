#lang racket

(require "queue.rkt")

; ---------------------------------------
; Agenda
; ---------------------------------------

(provide (all-defined-out))

; time segment
(define (make-time-segment time queue)
  (mcons time queue))
(define (segment-time s) (mcar s))
(define (segment-queue s) (mcdr s))

; agenda
(define (make-agenda) (mcons 0 null))
(define (current-time agenda) (mcar agenda))
(define (set-current-time! agenda time)
  (set-mcar! agenda time))
(define (segments agenda) (mcdr agenda))
(define (set-segments! agenda segments)
  (set-mcdr! agenda segments))
(define (first-segment agenda) (mcar (segments agenda)))
(define (rest-segments agenda) (mcdr (segments agenda)))

(define (empty-agenda? agenda) (null? (segments agenda)))

(define (display-segments agenda)
  (let ((segs (segments agenda)))
    (define (show-seg s)
      (display (format " [~a: " (segment-time (mcar s))))
      (print-queue (segment-queue (mcar s)))
      (displayln "]")
      (if (null? (mcdr s))
          (void)
          (show-seg (mcdr s))))
    (cond ((null? segs) (display "()"))
          (else
           (displayln "(")
           (show-seg segs)
           (displayln ")")))))

; add action to agenda
(define (add-to-agenda! time action agenda)

  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (mcar segments)))))

  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= (segment-time (mcar segments)) time)
        (insert-queue! (segment-queue (mcar segments))
                       action)
        (let ((rest (mcdr segments)))
          (if (belongs-before? rest)
              (set-mcdr!
               segments
               (mcons (make-new-time-segment time action)
                      (mcdr segments)))
              (add-to-segments! rest)))))

  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (mcons (make-new-time-segment time action)
                segments))
        (add-to-segments! segments))))

; remove first agenda item
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda))
        (void))))

; First agenda item
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;;; Default agenda

(define the-agenda (make-agenda))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda)) action the-agenda))

(define (propagate)
  (cond
    ((empty-agenda? the-agenda) 'done)
    (else ((first-agenda-item the-agenda))
          (remove-first-agenda-item! the-agenda)
          (propagate))))

;;; Test

;(add-to-agenda! 0 (lambda() (displayln 'a)) the-agenda)
;(after-delay 1 (lambda() (displayln 'b)))
;(after-delay 1 (lambda() (displayln 'c)))
;(after-delay 5 (lambda() (displayln 'd)))
;(after-delay 4 (lambda() (displayln 'e)))
;(after-delay 4 (lambda() (displayln 'f)))
;(after-delay 4 (lambda() (displayln 'g)))
;(displayln "Agenda:")
;(display-segments the-agenda)
;(displayln "\nPropagate...")
;(propagate)
