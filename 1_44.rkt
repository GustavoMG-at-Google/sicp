#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (id x) x)

(define (repeated f n)
  (if (= n 0)
    id
    (compose f (repeated f (- n 1)))))

(define (smooth f)
  (define dx 1.0)
  (lambda (x)
    (let ((prev (- x dx))
          (next (+ x dx)))
      (/ (+ (f prev) (f x) (f next)) 3))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(define (square x) (* x x))

((n-fold-smooth square 2) 5)

