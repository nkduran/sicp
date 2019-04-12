#lang racket

(define (divides? a b)
  (= (remainder b a) 0))

(define (cont-frac make-n make-d k)
  (define (cont-frac-iter i)
    (if (> i k)
      0
      (/ (make-n i) (+ (make-d i) (cont-frac-iter (+ i 1))))))
  (cont-frac-iter 0))

(define (make-n i)
  1.0)

(define (make-d i)
  (cond ((divides? 3 (+ i 1)) (* (/ (+ i 1) 3) 2))
        (else 1.0)))

(define result (+ (cont-frac make-n make-d 100) 2.0))

result