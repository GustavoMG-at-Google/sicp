#lang racket

(define (even? n)
  (= (remainder n 2) 0))

(define (miller-rabin n)
  (define (multiply a b)
    (remainder (* a b) n))
  (define (square x)
    (multiply x x))
  (define (test base)
    (define (square-if-nontrivial num)
      (if (and (= (square num) 1) (< 1 num) (< num (- n 1)))
        0
        (square num)))
    (define (binexp exp)
      (cond
        ((= exp 0) 1)
        ((even? exp) (square-if-nontrivial (binexp (/ exp 2))))
        (else (multiply base (binexp (- exp 1))))))
    (= (binexp (- n 1)) 1))
  (define (test-many-times ntimes)
    (cond ((= ntimes 0) true)
          ((test (+ 1 (random (- n 1)))) (test-many-times (- ntimes 1)))
          (else false)))
  (test-many-times 5))

(define (print-if-prime p)
  (define (print-prime)
    (print p)
    (newline))
  (when (miller-rabin p) (print-prime)))

(define (print-primes-to n)
  (define (iter i)
    (print-if-prime i)
    (when (< i n) (iter (+ i 1))))
  (iter 2))

(print-primes-to 100)
(print-if-prime 561)
