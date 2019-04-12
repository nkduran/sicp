#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound x) (min (car x) (cdr x)))

(define (upper-bound x) (max (car x) (cdr x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (let ((y-upper (upper-bound y))
        (y-lower (lower-bound y)))
    (cond ((or (= y-upper 0) (= y-lower 0) (and (> y-upper 0) (< y-lower 0))) (display "y range is including 0"))
          (else (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                               (/ 1.0 (lower-bound y))))))))
                         
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (print-interval x)
  (newline)
  (display "[")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display "]"))


(define x1 (make-interval 6 8))
(define x2 (make-interval 20 10))
(define x3 (make-interval -3 9))
(define r1 (add-interval x1 x2))
(define r2 (mul-interval x1 x2))
(define r3 (div-interval x1 x2))
(define r4 (sub-interval x1 x2))
(define r5 (div-interval x1 x3))

(print-interval r1)
(print-interval r2)
(print-interval r3)
(print-interval r4)