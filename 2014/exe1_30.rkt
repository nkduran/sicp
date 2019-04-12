#lang racket
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (new-sum term a next b)
  (define (sum-iter a result)
    (if (> a b)
        result
        (sum-iter (next a)
                  (+ (term a) result))))
  (sum-iter a 0))
      

(define (cube x) (* x x x))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (next x)
    (+ x (* 2 h)))
  (define (func x)
    (+ (f x) (* 4 (f (+ x h))) (f (next x))))
  (* (/ h 3) (new-sum func a next b)))

(integral cube 0 1.0 100)
(integral cube 0 1.0 1000)