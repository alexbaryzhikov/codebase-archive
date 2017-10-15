#lang racket

(require rackunit)
(provide eval-ed apply-ed)

;; Evaluate expression in environment
(define (eval-ed exp env)
  (cond ((number? exp) exp)
        ((boolean? exp) exp)
        ((string? exp) exp)
        ((primitive? exp) exp)
        ((symbol? exp) (lookup exp env))
        ((eq? (car exp) 'lambda) (list 'closure (cdr exp) env))
        ((eq? (car exp) 'cond) (evcond (cdr exp) env))
        (else (apply-ed (eval-ed (car exp) env) (evlist (cdr exp) env)))))

;; Apply function to arguments
(define (apply-ed func args)
  (cond ((primitive? func) (apply func args))
        ((eq? (car func) 'closure)
         (eval-ed (cadadr func) (bind (caadr func) args (caddr func))))
        (else (error "not a function"))))

;; Evaluate list of arguments
(define (evlist args env)
  (cond ((eq? args NIL) NIL)
        (else (list* (eval-ed (car args) env) (evlist (cdr args) env)))))

;; Evaluate conditional expression
(define (evcond clauses env)
  (cond ((eq? clauses NIL) NIL)
        ((eq? (caar clauses) 'else) (eval-ed (cadar clauses) env))
        ((false? (eval-ed (caar clauses) env)) (evcond (cdr clauses) env))
        (else (eval-ed (cadar clauses) env))))

;; Bind variables to values
(define (bind vars vals env)
  (list* (pair-up vars vals) env))

;; Make a list of variable:value pairs
(define (pair-up vars vals)
  (cond ((eq? vars NIL)
         (cond ((eq? vals NIL) NIL)
               (else (error "too many arguments"))))
        ((eq? vals NIL) (error "too few arguments"))
        (else (list* (cons (car vars) (car vals))
                     (pair-up (cdr vars) (cdr vals))))))

;; Lookup symbol in environment
(define (lookup sym env)
  (cond ((eq? env NIL) (error "unbound variable"))
        (else
         ((λ (item)
            (cond ((eq? item NIL) (lookup sym (cdr env)))
                  (else (cdr item))))
          (assq sym (car env))))))

;; Lookup key in a list of key:value pairs
(define (assq key lst)
  (cond ((eq? lst NIL) NIL)
        ((eq? key (caar lst)) (car lst))
        (else (assq key (cdr lst)))))

(define NIL '())
(define GLOBAL-ENV (list (list (cons '+ +) (cons '- -) (cons '* *) (cons '/ /))))


;; Tests
(check-equal? (assq 'x GLOBAL-ENV) NIL)
(check-equal? (assq 'x '((x 42))) '(x 42))
(check-equal? (assq 'x '((y 2))) NIL)
(check-equal? (assq 'n '((y 3) (n 6))) '(n 6))
(check-equal? (lookup '+ (list (list (cons '+ +) (cons '- -)))) +)
(check-equal? (lookup 'x (list (list (cons 'n 4) (cons 'y 0)) (list (cons 'x 41)))) 41)
(check-equal? (pair-up NIL NIL) NIL)
(check-equal? (pair-up '(x y z) '(1 2 3)) '((x . 1) (y . 2) (z . 3)))
(check-equal? (bind '(x y) '(10 -4) (list (list (cons NIL NIL)))) '(((x . 10) (y . -4)) ((()))))
(check-equal? (evlist (list pi 4 "abc") GLOBAL-ENV) '(3.141592653589793 4 "abc"))
(check-equal? (evcond NIL GLOBAL-ENV) NIL)
(check-equal? (evcond '((else 42)) GLOBAL-ENV) 42)
(check-equal? (evcond '((#f 32) (#t 10) (else 99)) GLOBAL-ENV) 10)
(check-equal? (eval-ed 42 NIL) 42)
(check-equal? (eval-ed '(lambda () (42)) NIL) '(closure (() (42)) ()))
(check-equal? (apply-ed (list 'closure '((x y) (+ x y)) GLOBAL-ENV) '(4 29)) 33)
(check-equal? (eval-ed '((lambda (x y z) (+ (* x y) z)) 3 5 22) GLOBAL-ENV) 37)
