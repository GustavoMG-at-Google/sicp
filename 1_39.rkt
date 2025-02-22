#lang racket

(define (K a b c)
    (/ a (+ b c)))

(define (pow a n)
  (define (impl i)
    (if (= i 0)
      1
      (* a (pow a (- i 1)))))
  (impl n))

(define (cont-fracc n d k)
  (define (impl acc i)
    (if (= i 0)
      acc
      (impl (K (n i) (d i) acc) (- i 1))))
  (+ (d 0) (impl 0 k)))

(define (continuant-tan k x)
  (define (n i)
    (if (= i 1)
      x
      (- (pow x i))))
  (define (d i)
    (if (= i 0)
      0
      (- (* 2 i) 1)))
  (cont-fracc n d k))

(define (print-range f k)
  (define (iter i)
    (display (f i))
    (newline)
    (when (< i k) (iter (+ i 1))))
  (iter 1))

(print-range
  (lambda (i) (continuant-tan i (/ pi 4)))
  10)
