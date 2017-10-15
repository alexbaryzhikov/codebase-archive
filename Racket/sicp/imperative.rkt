#lang racket

; Subject: imperative programming

; substitution does not work for programs with assgnment

(define count 1)
(define (demo x)
  (set! count (add1 count))
  (+ x count))


(define (fact n)
  (let ((i 1) (m 1))
    (define (loop)
      (cond ((> i n) m)
            (else
             (set! m (* i m))
             (set! i (+ i 1))
             (loop))))
    (loop)))


; Bound Variables
;
; We say that a variable, V, is "bound in
; an expression", E, if the meaning of E
; is unchanged by the uniform replacement
; of a variable, W, not occurring in E,
; for every occurrance of V in E

; y and x are bound, but in different expressions
(lambda (y) ((lambda(x) (* x y)) 3))


; Free Variables
;
; We say that a variable, V, is "free in
; an expression", E, if the meaning of E
; is changed by the uniform replacement of
; a variable, W, not occurring in E, for
; every occurrance of V in E.

; y is free
(lambda (x) (* x y))


; Scope
;
; If X is a bound variable in E then there
; is a lambda expression where it is
; bound. We call the list of formal
; parameters of the lambda expression the
; "bound variable list" and we say that
; the lambda expression "binds" the
; variables "declared" in its bound
; variable list. In addition, these parts
; of the expression where a variable has a
; value defined by the lambda expression
; which binds it is called the "scope" of
; the variable


; Evaluation rules
;
; Rule 1: A procedure object is applied to
; a set of arguments by constructing a
; frame, binding the formal parameters of
; the procedure to the actual arguments of
; the call, and then evaluating the body
; of the procedure in the context of the
; new environment constructed. The new
; frame has as its enclosing environment
; the environment part of the procedure
; object being applied
;
; Rule 2: A lambda-expression is evaluated
; relative to a given environment as
; follows: a new procedure object is
; formed, combining the text (code) of the
; lambda-expression with a pointer to the
; environment of evaluation.


