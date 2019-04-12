#lang racket

(define (cont-frac make-n make-d k)
  (define (cont-frac-iter i)
    (if (> i k)
      0
      (/ (make-n i) (+ (make-d i) (cont-frac-iter (+ i 1))))))
  (cont-frac-iter 0))

(define tolerance 0.00001)
(define (cont-frac-guess make-n make-d)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (cont-frac-iter i guess)
    (let ((next (/ (make-n i) (+ (make-d i) guess))))
      (newline)
      (display guess)
      (if (close-enough? guess next)
          next
          (cont-frac-iter (+ i 1) next))))
  (cont-frac-iter 1 0))

(define (new-cont-frac make-n make-d k)
  (define (cont-frac-iter i result)
    (if (< i 1)
        result
        (cont-frac-iter (- i 1) (/ (make-n i) (+ (make-d i) result)))))
  (cont-frac-iter k 0))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
(new-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
(newline)
(cont-frac-guess (lambda (i) 1.0) (lambda (i) 1.0))