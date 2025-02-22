#lang racket

(define (filtered-accumulate include? combiner null-value term a next b)
  (define (maybe-combine res i)
    (if (include? i)
      (combiner res (term i))
      res))
  (define (iter i res)
    (if (> i b)
      res
      (iter (next i) (maybe-combine res i))))
  (iter a null-value))

(define (prime? n)
  (define (iter d)
    (cond
      ((> (* d d) n) true)
      ((= (remainder n d) 0) false)
      (else (iter (+ d 1)))))
  (if (= n 1) false (iter 2)))

(define (square x) (* x x))

(define (id x) x)

(define (inc x) (+ x 1))

(define (sum-squares-primes-in-range a b)
  (filtered-accumulate prime? + 0 square a inc b))

(sum-squares-primes-in-range 1 10)

(define (prod-relatively-prime n)
  (define (relatively-prime? x)
    (= (gcd x n) 1))
  (filtered-accumulate relatively-prime? * 1 id 2 inc (- n 1)))

(prod-relatively-prime 10)
