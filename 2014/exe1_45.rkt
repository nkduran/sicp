#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1)) f)))

(define (power n)
  (lambda (x)
    (if (= n 1)
        x
        (* x ((power (- n 1)) x)))))

((power 8) 2.0)

(define (root n)
  (lambda (x)
    (fixed-point (repeated (average-damp (lambda (y) (/ x ((power (- n 1)) y)))) (/ n 2))
                 1.0)))

((root 8) 256.0)
