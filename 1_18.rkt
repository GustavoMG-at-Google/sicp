#lang racket

(define (even? n) (= (remainder n 2) 0))
(define (double n) (* n 2))
(define (half n) (/ n 2))

(define (fast-mul a b)
  (define (iter acc a b)
    (cond ((= b 0) acc)
          ((even? b) (iter acc (double a) (half b)))
          (else (iter (+ acc a) (double a) (half (- b 1))))))
  (iter 0 a b))

(fast-mul 13 77)
