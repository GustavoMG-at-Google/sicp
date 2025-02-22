#lang racket

(define (accumulate combiner null-value term a next b)
  (define (rec i)
    (if (> i b)
      null-value
      (combiner (term i) (rec (next i)))))
  (define (iter i res)
    (if (> i b)
      res
      (iter (next i) (combiner res (term i)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (prod term a next b)
  (accumulate * 1 term a next b))

(define (id x) x)

(define (inc x) (+ x 1))

(sum id 1 inc 5)
(prod id 1 inc 5)
