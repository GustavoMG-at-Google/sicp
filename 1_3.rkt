#lang racket

(define (square x) (* x x))

(define (sum_of_squares x y) (+ (square x) (square y)))

(define (is_first_smallest? x y z)
  (and (<= x y) (<= x z)))

(define (sum_of_square_of_two_largest x y z)
  (cond ((is_first_smallest? x y z) (sum_of_squares y z))
        ((is_first_smallest? y z x) (sum_of_squares z x))
        (else (sum_of_squares x y))))

(sum_of_square_of_two_largest 5 4 3)
