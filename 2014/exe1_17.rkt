#lang racket

(define (fast-multi a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-multi (double a) (halve b)))
        (else (+ a (fast-multi a (- b 1))))))

(define (double a)
  (* a 2))

(define (halve a)
  (/ a 2))

(fast-multi 9 7)