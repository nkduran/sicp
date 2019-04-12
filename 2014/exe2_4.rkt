#lang racket

(define (new-cons x y)
  (lambda (m) (m x y)))

(define (new-car z)
  (z (lambda (p q) p)))

(define (new-cdr z)
  (z (lambda (p q) q)))

(new-car (new-cons 10 15))
(new-cdr (new-cons 8 9))