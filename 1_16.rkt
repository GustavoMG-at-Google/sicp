#lang racket

(define (even? n) (= (remainder n 2) 0))

(define (fast-exp b n)
  (define (iter acc b n)
    (cond ((= n 0) acc)
          ((even? n) (iter acc (* b b) (/ n 2)))
          (else (iter (* acc b) (* b b) (/ (- n 1) 2)))))
  (iter 1 b n))

(fast-exp 3 5)
