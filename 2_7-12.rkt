#lang racket

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

; width = width x + width y
(define (add-interval x y)
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define (is-positive? interval)
  (< 0 (lower-bound interval)))

(define (is-negative? interval)
  (> 0 (upper-bound interval)))

(define (spans-0? interval)
  (not (or (is-positive? interval) (is-negative? interval))))

; width is not defined by width x and width y, e.g.
; width (1,2) * (1,2) = width (1,4) = 3
; width (2,3) * (1,2) = width (2,6) = 4
(define (mul-interval x y)
  (cond
    ((and (is-positive? x) (is-positive? y))
      (make-interval (* (lower-bound x) (lower-bound y))
                     (* (upper-bound x) (upper-bound y))))
    ((and (is-positive? x) (is-negative? y))
      (make-interval (* (upper-bound x) (lower-bound y))
                     (* (lower-bound x) (upper-bound y))))
    ((and (is-positive? x) (spans-0? y))
      (make-interval (* (upper-bound x) (lower-bound y))
                     (* (upper-bound x) (upper-bound y))))
    ((and (is-negative? x) (is-positive? y))
      (make-interval (* (lower-bound x) (upper-bound y))
                     (* (upper-bound x) (lower-bound y))))
    ((and (is-negative? x) (is-negative? y))
      (make-interval (* (upper-bound x) (upper-bound y))
                     (* (lower-bound x) (lower-bound y))))
    ((and (is-negative? x) (spans-0? y))
      (make-interval (* (lower-bound x) (upper-bound y))
                     (* (lower-bound x) (lower-bound y))))
    ((and (spans-0? x) (is-positive? y))
      (make-interval (* (lower-bound x) (upper-bound y))
                     (* (upper-bound x) (upper-bound y))))
    ((and (spans-0? x) (is-negative? y))
      (make-interval (* (upper-bound x) (lower-bound y))
                     (* (lower-bound x) (lower-bound y))))
    ((and (spans-0? x) (spans-0? y))
     (let ((ub-1 (* (lower-bound x) (lower-bound y)))
           (ub-2 (* (upper-bound x) (upper-bound y)))
           (lb-1 (* (lower-bound x) (upper-bound y)))
           (lb-2 (* (upper-bound x) (lower-bound y))))
       (make-interval (min lb-1 lb-2) (max ub-1 ub-2))))))

; width is not defined by width x and width y, e.g.
; width (1,2) / (1,2) = width (0.5,2) = 2.5
; width (2,3) / (1,2) = width (1,3) = 2
(define (div-interval x y)
  (define (log-and-fail)
    (display "Error, denominator interval spans 0")
    (make-interval 0 0))
  (if (spans-0? y)
    (log-and-fail)
    (mul-interval
      x
      (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))

; width = width x + width y
(define (sub-interval x y)
  (add-interval
    x
    (make-interval (- (upper-bound y))
                   (- (lower-bound y)))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

(define (make-center-percent c p)
  (make-center-width c (* p c)))
(define (percent i)
  (/ (width i) (center i)))

(define x (make-center-percent 2 0.5))

(center (add-interval x x))
(percent (add-interval x x))

; In positive intervals
; center x * y = gm(upper-bound x, lower-bound x) * gm(upper-bound y, lower-bound y)
; gm(c*(1-p), c*(1+p)) = c*gm(1-p,1+p) = c * gm(1-p,1+p)
; center x * y = center x * center y * gm(1-p x, 1+p x) * gm(1-p y, 1+p y)
; upper-bound x * y = center x * center y * (1+percent x) * (1+percent y)
; percent x * y = sqrt(1+percent x * 1+percent y / 1-percent x / 1-percent y)
