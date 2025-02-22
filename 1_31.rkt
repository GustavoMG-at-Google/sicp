#lang racket

(define (product-rec term a next b)
  (if (> a b)
    1
    (* (term a) (product-rec term (next a) next b))))

(define (id x) x)

(define (inc x) (+ x 1))

(define (factorial n)
  (product-rec id 1 inc n))

(factorial 6)

(define (wallis n)
  (define (term i)
    (define a (* 2 i))
    (* (/ a (+ a 1)) (/ (+ a 2) (+ a 1))))
  (* 4 (product-rec term 1.0 inc n)))

(wallis 100)

(define (product-iter term a next b)
  (define (iter i acc)
    (if (> i b)
      acc
      (iter (next i) (* acc (term i)))))
  (iter a 1))

(product-iter id 1 inc 6)
