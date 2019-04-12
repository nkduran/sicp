#lang racket

(define (fast-multi a b)
  (fast-multi-iter a b 0))

(define (fast-multi-iter a counter product)
  (cond ((= counter 0) 0)
        ((even? counter) (fast-multi-iter (double a) (halve counter) product))
        (else (+ a (fast-multi-iter a (- counter 1) (+ a product))))))

(define (double a)
  (* a 2))

(define (halve a)
  (/ a 2))

(fast-multi 9 7)