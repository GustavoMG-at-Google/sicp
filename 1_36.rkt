#lang racket

(define (avg x y)
  (/ (+ x y) 2.0))

(define tolerance 0.00001)

(define (fixed-point f first-guess dampened)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (define next
      (if dampened
        (avg guess (f guess))
        (f guess)))
    (display guess)
    (newline)
    (if (close-enough? guess next)
      next
      (try next)))
  (try first-guess))

(fixed-point
  (lambda (x) (/ (log 1000) (log x)))
  2.0
  false)

(newline)

(fixed-point
  (lambda (x) (/ (log 1000) (log x)))
  2.0
  true)
