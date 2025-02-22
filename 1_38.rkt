#lang racket

(define (K a b c)
    (/ a (+ b c)))

(define (cont-fracc n d k)
  (define (impl acc i)
    (if (= i 0)
      acc
      (impl (K (n i) (d i) acc) (- i 1))))
  (+ (d 0) (impl 0 k)))

(define (continuant-e k)
  (define (n i) 1.0)
  (define (d i)
    (cond
      ((= i 0) 2.0)
      ((= (remainder i 3) 2) (* (/ (+ i 1) 3) 2))
      (else 1.0)))
  (cont-fracc n d k))

(define (print-range f k)
  (define (iter i)
    (display (f i))
    (newline)
    (when (< i k) (iter (+ i 1))))
  (iter 1))

(print-range continuant-e 10)
