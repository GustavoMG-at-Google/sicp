#lang racket

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-segment point-a point-b) (cons point-a point-b))
(define (start-point segment) (car segment))
(define (end-point segment) (cdr segment))

(define (average x y) (/ (+ x y) 2.0))

(define (mid-point segment)
  (make-point
    (average (x-point (start-point segment))
             (x-point (end-point segment)))
    (average (y-point (start-point segment))
             (y-point (end-point segment)))))

(define (print-point point)
  (newline)
  (display "(")
  (display (x-point point))
  (display ", ")
  (display (y-point point))
  (display ")"))

(print-point (mid-point (make-segment (make-point 1 3)
                                      (make-point 2 4))))
