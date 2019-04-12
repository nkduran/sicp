#lang racket
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (new-accumulate combiner null-value term a next b)
  (define (accumulate-iter a result)
    (if (> a b)
        result
        (accumulate-iter (next a)
                         (combiner (term a) result))))
  (accumulate-iter a null-value))

(define (add x y) (+ x y))
  
(define (sum term a next b)
  (accumulate add 0 term a next b))

(define (new-sum term a next b)
  (new-accumulate add 0 term a next b))

(define (multi x y) (* x y))

(define (product term a next b)
  (accumulate multi 1 term a next b))

(define (new-product term a next b)
  (accumulate multi 1 term a next b))

(define (next n)
  (+ n 1))

(define (identity n) n)

(sum identity 1 next 100)
(new-sum identity 1 next 100)

(product identity 1 next 6)
(new-product identity 1 next 6)