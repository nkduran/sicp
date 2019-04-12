#lang racket
(define (make-point x y)
  (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment px py)
  (cons px py))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (cdr line))

(define (midpoint-segment line)
  (make-point (/ (+ (x-point (start-segment line))
                    (x-point (end-segment line))) 2)
              (/ (+ (y-point (start-segment line))
                    (y-point (end-segment line))) 2)))

(define line (make-segment 
              (make-point 5.0 8.0)
              (make-point 7.0 11.0)))

(print-point (midpoint-segment line))