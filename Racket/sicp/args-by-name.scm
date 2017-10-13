#lang racket

; passing arguments by name (not by value)


(define (unless p c a)
  (cond ((not p) c)
        (else a)))

(unless (= 1 0) 2 (/ 1 0))
; supposed to transform to
(cond ((not (= 1 0)) 2)
      (else (/ 1 0)))

(define (unless p (name c) (name a))
  (cond ((not p) c)
        (else a)))


(define eval
  (lambda (exp env)
    (cond
      ((number? exp) exp)
      ((symbol? exp) (lookup exp env))
      ((eq? (car exp) 'quote) (cadr exp))
      ((eq? (car exp) 'lambda)
       (list 'closure (cdr exp) env))
      ((eq? (car exp) 'cond)
       (evcond (cdr exp) env))
      (else
       (apply (undelay (eval (car exp) env))
              (cdr exp)
              (evlist (cdr exp) env))))))

(define apply
  (lambda (proc ops args)
    (cond ((primitive? proc)
           (apply-primop proc
                         (evlist ops env)))
          ((eq? (car proc) 'closure)
           (eval (cadadr proc)
                 (bind (vnames (caadr proc))
                       (gevlist (caadr proc)
                                ops
                                env)
                       (caddr proc))))
          (else error-unknown-procedure))))

(define evlist
  (lambda (l env)
    (cond
      ((eq? l '()) '())
      (else
       (cons (undelay (eval (car l) env))
             (evlist (cdr l) env))))))

(define gevlist
  (lambda (vars exps env)
    (cond
      ((eq? exps '()) '())
      ((symbol? (car vars))
       (cons (eval (car exps) env)
             (gevlist (cdr vars)
                      (cdr exps)
                      env)))
      ((eq? (caar vars) 'name)
       (cons (make-delay (car exps) env)
             (gevlist (cdr vars)
                      (cdr exps)
                      env)))
      (else error-unknown-declaration))))

(define evcond
  (lambda (clauses env)
    (cond
      ((eq? clauses '()) '())
      ((eq? (caar clauses) 'else)
       (eval (cadar clauses) env))
      ((false? (undelay
                (eval (caar clauses) env)))
       (evcond (cdr clauses) env))
      (else
       (eval (cadar clauses) env)))))

(define false?
  (lambda (x) (eq? x nil)))

(define make-delay
  (lambda (exp env)
    (cons 'thunk (cons exp env))))

(define (undelay v)
  (cond ((pair? v)
         (cond ((eq? (car v) 'thunk)
                (undelay
                 (eval (cadr thunk)
                       (cddr thunk))))
               (else v)))
        (else v)))

