#lang racket
(require racket/include)
(include "agenda.rkt")
(include "queue.rkt")

; Subject: Full adder sumilation using event-driven system

; ---------------------------------------
; Means of abstraction
; ---------------------------------------

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; ---------------------------------------
; Implementing primitives
; ---------------------------------------

; NOT (inverter)

(define (inverter in out)
  (define (invert-in)
    (let ((new
           (logical-not (get-signal in))))
      (after-delay inverter-delay
         (lambda()
           (set-signal! out new)))))
  (add-action! in invert-in))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else
         (error "invalid signal" s))))

; AND gate

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1)
                        (get-signal a2))))
      (after-delay and-gate-delay
         (lambda()
           (set-signal! output
                        new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure))

(define (logical-and s1 s2)
  (if (and (or (= s1 1) (= s1 0))
           (or (= s2 1) (= s2 0)))
      (if (and (= s1 1) (= s2 1))
          1
          0)
      (error (format "invalid signal ~a or ~a" s1 s2))))

; OR gate

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1)
                       (get-signal a2))))
      (after-delay or-gate-delay
         (lambda()
           (set-signal! output
                        new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure))

(define (logical-or s1 s2)
  (if (and (or (= s1 1) (= s1 0))
           (or (= s2 1) (= s2 0)))
      (if (or (= s1 1) (= s2 1))
          1
          0)
      (error (format "invalid signal ~a or ~a" s1 s2))))

; dealy constants

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

; wire

(define (make-wire)
  (let ((signal 0) (action-procs '()))
    (define (set-my-signal! new)
      (cond ((= signal new) 'done)
            (else
             (set! signal new)
             (call-each action-procs))))
    (define (accept-action-proc proc)
      (set! action-procs
            (cons proc action-procs))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal)
            ((eq? m 'set-signal!)
             set-my-signal!)
            ((eq? m 'add-action!)
             accept-action-proc)
            (else
             (error "Bad message" m))))
    dispatch))

(define (call-each procedures)
  (cond ((null? procedures) 'done)
        (else
         ((car procedures))
         (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-proc)
  ((wire 'add-action!) action-proc))

; probe

(define (probe name wire)
  (add-action! wire
               (lambda()
                 (displayln (format "~a ~a New-value = ~a" name (current-time the-agenda) (get-signal wire))))))

; ---------------------------------------
; 4-bit adder example setup
; ---------------------------------------

(define A0 (make-wire))
(define A1 (make-wire))
(define A2 (make-wire))
(define A3 (make-wire))
(define B0 (make-wire))
(define B1 (make-wire))
(define B2 (make-wire))
(define B3 (make-wire))
(define C0 (make-wire))
(define C1 (make-wire))
(define C2 (make-wire))
(define S0 (make-wire))
(define S1 (make-wire))
(define S2 (make-wire))
(define S3 (make-wire))
(define S4 (make-wire))

(probe 'sum-0 S0)
(probe 'sum-1 S1)
(probe 'sum-2 S2)
(probe 'sum-3 S3)
(probe 'sum-4 S4)

(half-adder A0 B0 S0 C0)
(full-adder A1 B1 C0 S1 C1)
(full-adder A2 B2 C1 S2 C2)
(full-adder A3 B3 C2 S3 S4)


