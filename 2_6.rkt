#lang racket

(define zero
  (lambda (f)
    (lambda (x) x)))
(define one
  (lambda (f)
    (lambda (x) (f x))))
(define two
  (lambda (f)
    (lambda (x) (f (f x)))))
(define (add m n)
  (lambda (f)
    (lambda (x) ((m f) ((n f) x)))))

(define (testing x)
  (display "|")
  x)

((zero testing) 0)
((one testing) 0)
((two testing) 0)
