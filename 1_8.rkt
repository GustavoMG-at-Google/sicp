#lang racket

(define (cubert x)
  (define eps 0.001)
  (define (iter guess)
    (define new_guess
      (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
    (define good-enough?
      (< (abs (- new_guess guess)) eps))
    (if good-enough?
      guess
      (iter new_guess)))
  (iter 1.0))

(cubert 64)
