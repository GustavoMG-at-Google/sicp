#lang racket

(define (calc x y z) (+ (* 3 x) (* 2 y) x))

(define (f n)
  (if (< n 3)
    n
    (calc (f (- n 3)) (f (- n 2)) (f (- n 1)))))

(define (g n)
  (define (iter x y z n target)
    (if (= n target)
      z
      (iter y z (calc x y z) (+ n 1) target)))
  (if (< n 3)
    n
    (iter 0 1 2 2 n)))

(f 7)
(g 7)
