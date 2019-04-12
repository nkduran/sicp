#lang racket

(define (func n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 2)
        (else (+ (func (- n 1))
                 (* 2 (func (- n 2)))
                 (* 3 (func (- n 3)))))))

(func 9)

(define (func-1 n)
  (func-iter 2 1 0 n))

(define (func-iter a b c count)
  (cond ((= count 0) c)
        ((= count 1) b)
        ((= count 2) a)
        (else (func-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))

(func-1 9)