#lang racket

(define (double f)
  (lambda (x)
    (f (f x))))

((double (lambda (x) (+ x 1))) 2)

(define (inc x)
  (+ x 1))

(((double (double double)) inc) 5)
((double (double (double inc))) 5)