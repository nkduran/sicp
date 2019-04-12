#lang racket

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (cond ((= counter 0) product)
        ((even? counter) (expt-iter (* b b) (/ counter 2) product))
        (else (expt-iter b (- counter 1) (* b product)))))

(expt 2 11)