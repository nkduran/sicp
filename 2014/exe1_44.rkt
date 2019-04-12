#lang racket

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1)) f)))

(define dx 0.000001)

(define (average-3 x y z)
  (/ (+ x y z) 3))

(define (smooth f)
  (lambda (x)
    (average-3 (f (- x dx))
               (f x)
               (f (+ x dx)))))

(define (n-smooth f n)
  (repeated (smooth f) n))

(define (square x)
  (* x x))

((n-smooth square 10) 1.0) 