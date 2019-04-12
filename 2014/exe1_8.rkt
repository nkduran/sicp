#lang racket

(define (cube-root-iter old-guess guess x)
  (if (good-enough? old-guess guess x)
      guess
      (cube-root-iter guess (improve guess x)
                 x)))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (good-enough? old-guess guess x)
  (< (abs (- old-guess guess))
     (* guess 0.001)))

(define (cube-root x)
  (cube-root-iter 1.0 2.0 x))

(cube-root 27.0)
