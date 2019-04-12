#lang racket

(define (power x y)
  (define (power-iter n result)
    (if (= n 0)
        result
        (power-iter (- n 1) (* x result))))
  (power-iter y 1))

(power 3 4)

(define (divider x y)
  (define (divide-iter dividend result)
    (if (not (= (remainder dividend y) 0))
        result
        (divide-iter (/ dividend y) (+ result 1))))
  (divide-iter x 0))

(divider 81 3)

(define (new-cons a b)
  (* (power 2 a) (power 3 b)))

(define (new-car x)
  (divider x 2))

(define (new-cdr x)
  (divider x 3))

(new-car (new-cons 3 4))
(new-cdr (new-cons 6 8))
