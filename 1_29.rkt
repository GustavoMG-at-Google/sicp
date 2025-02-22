#lang racket

(define (even? n) (= (remainder n 2) 0))

(define (inc n) (+ n 1))

(define (sum term a next b)
  (if (< b a)
    0
    (+ (term a) (sum term (next a) next b))))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (cte k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((even? k) 2)
          (else 4)))
  (define (sumand k)
    (* (cte k) (y k)))
  (* (/ h 3) (sum sumand 0 inc n)))

(define (cube n) (* n n n))

(simpson-integral cube 0.0 1.0 1000)
