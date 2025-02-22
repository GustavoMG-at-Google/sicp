#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (id x) x)

(define (repeated f n)
  (if (= n 0)
    id
    (compose f (repeated f (- n 1)))))

(define (avg x y)
  (/ (+ x y) 2.0))

(define tolerance 0.00001)

(define (damp f)
(lambda (x) (avg x (f x))))

(define (nth-damp f n)
  ((repeated damp n) f))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (define next (f guess))
    (if (close-enough? guess next)
      next
      (try next)))
  (try first-guess))

(define (pow x n)
  (if (= n 0) 1 (* x (pow x (- n 1)))))

(define (log2 n)
  (define (iter pow2 exp)
    (if (>= pow2 n) exp (iter (* 2 pow2) (+ 1 exp))))
  (iter 1 0))

(define (nth-root n y)
  (define n-damp (log2 n))
  (define (transform x)
    (/ y (pow x (- n 1))))
  (define f (nth-damp transform n-damp))
  (fixed-point f 1.0))

(nth-root 2 2)
(nth-root 3 2)
(nth-root 4 2)
(nth-root 7 2)
(nth-root 8 2)
(nth-root 15 2)
(nth-root 16 2)
