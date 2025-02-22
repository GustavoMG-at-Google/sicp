#lang racket

(define (cont-fracc n d k)
  (define (impl i)
    (if (> i k)
      0
      (/ (n k) (+ (d k) (impl (+ i 1))))))
  (impl 1))

(define (print-cont-fracc n d lim)
  (define (iter i)
    (display (cont-fracc n d i))
    (newline)
    (if (> i lim)
      0
      (iter (+ i 1))))
  (iter 1))

(print-cont-fracc
  (lambda (x) 1.0)
  (lambda (x) 1.0)
  10)
