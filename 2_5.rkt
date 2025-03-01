#lang racket

(define (pow base exp)
  (if (= exp 0)
    1
    (* base (pow base (- exp 1)))))

(define (divides? divisor number)
  (= (remainder number divisor) 0))

(define (nu number base)
  (if (divides? base number)
    (+ 1 (nu (/ number base) base))
    0))

(define (cons a b)
  (* (pow 2 a) (pow 3 b)))
(define (car z)
  (nu z 2))
(define (cdr z)
  (nu z 3))

(car (cons 4 5))
(cdr (cons 4 5))
