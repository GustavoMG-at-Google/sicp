#lang racket

(define (make-rat n d)
  (let ((g (gcd n d))
        (is-pos? (if (> n 0) (> d 0) (< d 0))))
    (cons
      (/ (if is-pos? (abs n) (- (abs n))) g)
      (/ (abs d) g))))
(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 6 9))
(print-rat (make-rat 6 -9))
(print-rat (make-rat -6 9))
(print-rat (make-rat -6 -9))
