#lang racket

(define (sum term a next b)
  (define (iter i result)
    (if (< b i)
      result
      (iter (next i) (+ result (term i)))))
  (iter a 0))

(define (inc-2 n) (+ n 2))

(define (cube n) (* n n n))

(sum cube 1 inc-2 5)
