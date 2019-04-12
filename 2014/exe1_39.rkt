#lang racket

(define (tan-cf x k)
  (define sqrx (* x x))
  (define (make-n i)
    (if (= i 1)
        x
        (* sqrx -1)))
  (define (make-d i)
    (- (* 2 i) 1))
  (define (cont-frac-iter i)
    (if (> i k)
      0
      (/ (make-n i) (+ (make-d i) (cont-frac-iter (+ i 1))))))
  (cont-frac-iter 1))

(define pi 3.1415926)
(tan-cf (/ pi 4) 20)