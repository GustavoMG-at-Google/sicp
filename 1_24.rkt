#lang racket

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp mod)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder
            (square (expmod base (/ exp 2) mod))
            mod))
        (else
          (remainder
            (* base (expmod base (- exp 1) mod))
            mod))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n ) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n)
  (define (iter times)
    (cond ((= times 0) true)
          ((fermat-test n) (iter (- times 1)))
          (else false)))
  (iter 20))

(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (when (fast-prime? n)
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
