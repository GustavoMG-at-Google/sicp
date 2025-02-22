#lang racket

(define (pascal row col)
  (define prev_row (- row 1))
  (define prev_col (- col 1))
  (define is_boundary?
    (or (= col 0) (= col row)))
  (if is_boundary?
    1
    (+
      (pascal prev_row prev_col)
      (pascal prev_row col))))

(pascal 4 2)
