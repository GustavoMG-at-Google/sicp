#lang racket

(define (iterative-improvement improve good-enough?)
  (define (iter guess)
    (if (good-enough? guess)
      guess
      (iter (improve guess))))
  iter)

(define (sqrt n)
  (define (improve x)
    (/ (+ x (/ n x)) 2))
  (define (good-enough? x)
    (< (abs (- (* x x) n)) 0.00001))
  ((iterative-improvement improve good-enough?) 1.0))

(sqrt 2)

(define (fixed-point f guess)
  (define (improve x) (f x))
  (define (good-enough? x)
    (< (abs (- (f x) x)) 0.00001))
  ((iterative-improvement improve good-enough?) guess))

(fixed-point (lambda (x) (/ (+ x (/ 2.0 x)) 2.0)) 2.0)
