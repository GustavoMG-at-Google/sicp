#lang racket

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp mod)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder
            (square (expmod base (/ exp 2) mod))
            mod))
        (else
          (remainder
            (* base (expmod base (- exp 1) mod))
            mod))))

(define (fermat-test a n)
  (= (expmod a n n ) a))

(define (all f n)
  (define (iter i)
    (if (= i n)
      true
      (and (f i) (iter (+ i 1)))))
  (iter 0))

(define (fermat-test-for-all n)
  (define (fermat-test-for-n a)
    (fermat-test a n))
  (all fermat-test-for-n n))

; Carmichael numbers
(fermat-test-for-all 561)
(fermat-test-for-all 1105)
(fermat-test-for-all 1729)
(fermat-test-for-all 2465)
(fermat-test-for-all 2821)
(fermat-test-for-all 6601)
