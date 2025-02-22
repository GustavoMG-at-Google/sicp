#lang racket

(define (even? n) (= (remainder n 2) 0))
(define (double n) (* n 2))
(define (half n) (/ n 2))

(define (mul a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mul a (half b))))
        (else (+ a (double (mul a (half (- b 1))))))))

(mul 13 77)
