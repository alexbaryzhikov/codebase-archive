#lang racket/base
(require racket/generator)

(define my-generator
  (generator
   ()
   (let loop ([i 0])
     (begin
       (yield i)
       (loop (add1 i))))))
