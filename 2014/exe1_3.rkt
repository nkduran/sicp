#lang racket
(define (sum-square-largest x y z)
  (cond ((and (< x y) (< x z))
        (+ (* y y) (* z z)))
        (else (sum-square-largest y z x))))

(sum-square-largest 8 3 6)
  
        
(define (ssl x y z)
  (cond ((> z x) (ssl z y x))
        ((> z y) (ssl x z y))
        (else (+ (* x x) (* y y)))))

(ssl 8 3 6)