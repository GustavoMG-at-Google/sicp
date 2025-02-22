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
    (else (find-divisor (next test-divisor) n))))

(define (next test-divisor)
  (if (= test-divisor 2) 3 (+ test-divisor 2)))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (when (prime? n)
    (report n (time-since start-time))))

(define (time-since start-time)
  (- (current-inexact-milliseconds) start-time))

(define (report n elapsed-time)
  (display n)
  (display ", ")
  (display elapsed-time)
  (newline))

(define (iter f a b)
  (when (< a b)
      (f a) (iter f (+ a 2) b)))

(iter timed-prime-test 3 1000000)
