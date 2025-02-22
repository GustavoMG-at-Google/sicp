#lang racket

(define (square x)
  (* x x))

(define (divides? d n)
  (= (remainder n d) 0))

(define (smallest-divisor n)
  (find-divisor 2 n))

(define (find-divisor test-divisor n)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor (+ test-divisor 1) n))))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
