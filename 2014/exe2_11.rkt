#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound x) (min (car x) (cdr x)))

(define (upper-bound x) (max (car x) (cdr x)))

(define (mul-interval x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (cond ((and (>= x1 0) (>= y1 0)) (make-interval (* x1 y1) (* x2 y2))) 
          ((and (<= x2 0) (<= y2 0)) (make-interval (* x2 y2) (* x1 y1)))
          ((and (>= x1 0) (<= y2 0)) (make-interval (* x2 y1) (* x1 y2)))
          ((and (<= x2 0) (>= y1 0)) (make-interval (* x1 y2) (* x2 y1)))
          ((and (or (< x1 0) (> x2 0)) (>= y1 0)) (make-interval (* x1 y2) (* x2 y2)))
          ((and (or (< x1 0) (> x2 0)) (<= y2 0)) (make-interval (* x2 y1) (* x1 y1)))
          ((and (or (< x1 0) (> x2 0)) (or (< y1 0) (> y2 0)))) (make-interval (min (* x1 y2) (* x2 y1))
                                                                              (max (* x1 y1) (* x2 y2))))))