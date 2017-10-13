#lang racket

(define (merge x y)
  (cond
    ((null? x) y)
    ((null? y) x)
    (else
     (let ((a (car x)) (b (car y)))
       (if (< a b)
           (cons a
                 (merge (cdr x) y))
           (cons b
                 (merge x (cdr y))))))))

; =========================================================
; Logical programming
; =========================================================


(rule (merge-to-form () ?y ?y))
(rule (merge-to-form ?y () ?y))


(rule
 (merge-to-form
  (?a . ?x) (?b . ?y) (?b . ?z))
 (and (merge-to-form (?a . ?x) ?y ?z)
      (lisp-value > ?a ?b)))

(rule
 (merge-to-form
  (?a . ?x) (?b . ?y) (?a . ?z))
 (and (merge-to-form ?x (?b . ?y) ?z)
      (lisp-value > ?b ?a)))



; =========================================================
; Implementation
; =========================================================

; sample patterns
(a ?x c)
(job ?x (computer ?y))
(job ?x (computer . ?y))
(a ?x ?x)
(?x ?y ?y ?x)
(a . ?x)


(match pat data dictionary)

