#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (id x) x)

(define (repeated f n)
  (if (= n 0)
    id
    (compose f (repeated f (- n 1)))))

(define (square x) (* x x))

((repeated square 2) 5)


