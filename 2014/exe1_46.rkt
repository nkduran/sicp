#lang racket

(define (iterative-improve good-enough? improve)
  (lambda (guess x)
    (if (good-enough? guess x)
        guess
        ((iterative-improve good-enough? improve) (improve guess x)
                                                  x))))
(define (square x)
  (* x x))

(define (sqrt x)
  ((iterative-improve (lambda (guess x) (< (abs (- (square guess) x)) 0.00001)) 
                     (lambda (guess x) (/ (+ guess x) 2))) 1.0 x))

