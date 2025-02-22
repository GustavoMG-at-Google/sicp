#lang racket

(define (square x) (* x x))

(define eps 0.001)

(define (average x y) (/ (+ x y) 2.0))

(define (good-enough? prev_guess guess)
  (< (abs (- prev_guess guess)) eps))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter prev_guess guess x)
  (if (good-enough? prev_guess guess)
    guess
    (sqrt-iter guess (improve guess x) x)))

(define (sqrt x) (sqrt-iter 0.0 1.0 x))

(sqrt 0) ; new: 0.00097 old: 0.03125
