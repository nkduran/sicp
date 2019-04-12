#lang racket
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (new-product term a next b)
  (define (product-iter a result)
    (if (> a b)
        result
        (product-iter (next a)
                      (* (term a) result))))
  (product-iter a 1))

(define (square x)
  (* x x))

(define (double x)
  (* 2 x))

(define (next x)
  (+ x 1))

(define (f k)
    (/ (* (double k) (double (+ k 1)))
       (square (+ (double k) 1))))

(define (factorial a b)  
  (* 4 (product f a next b)))

(define (new-factorial a b)
  (* 4 (new-product f a next b)))
  
(factorial 1.0 100000)
(new-factorial 1.0 100000)