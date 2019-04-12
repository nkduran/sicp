#lang racket
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display guess)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (fun x)
  (/ (log 1000) (log x)))

(define (new-fun x)
  (define (average x y)
    (/ (+ x y) 2))
  (average x (/ (log 1000) (log x))))

(define (get-log)
  (fixed-point fun 10.0))

(define (new-get-log)
  (fixed-point new-fun 10.0))

(get-log)
(newline)
(new-get-log)