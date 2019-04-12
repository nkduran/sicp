#lang racket

#! sicp 2.2

(define (make-segment startp endp)
  (cons startp endp))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment segment)
  (define x (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2))
  (define y (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2))
  (make-point x y))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define startp1 3.0)
(define endp1 6.0)
(define startp2 10.0)
(define endp2 18.0)

(define l1 (make-segment (make-point startp1 endp1) (make-point startp2 endp2)))
(print-point (midpoint-segment l1))

#! sicp 2.3
(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (make-rect lt-point rb-point)
  (cons lt-point rb-point))

(define (lt-point rect)
  (car rect))

(define (rb-point rect)
  (cdr rect))

(define (width rect)
  (abs (- (x-point (lt-point rect)) (x-point (rb-point rect)))))

(define (height rect)
  (abs (- (y-point (lt-point rect)) (y-point (rb-point rect)))))

(define (perimeter rect)
  (* 2 (+ (width rect) (height rect))))

(define (area rect)
  (* (width rect) (height rect)))

(define one-rect (make-rect (make-point 1 10) (make-point 20 5)))
(perimeter one-rect)
(area one-rect)
