#lang racket

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

#| First representation
(define (make-rectangle point-a point-b)
  (cons point-a point-b))
(define (first-point rectangle)
  (car rectangle))
(define (second-point rectangle)
  (cdr rectangle))

(define (base rectangle)
  (abs (-(x-point (first-point rectangle))
         (x-point (second-point rectangle)))))
(define (height rectangle)
  (abs (-(y-point (first-point rectangle))
         (y-point (second-point rectangle)))))

(define rectangle (make-rectangle (make-point 0 0)
                                  (make-point 2 3)))
|#

(define (make-rectangle base height)
  (cons base height))
(define (base rectangle)
  (car rectangle))
(define (height rectangle)
  (cdr rectangle))

(define rectangle (make-rectangle 2 3))

; Common layer
(define (perimeter rectangle)
  (* (+ (base rectangle) (height rectangle)) 2))
(define (area rectangle)
  (* (base rectangle) (height rectangle)))

(perimeter rectangle)
(area rectangle)
