#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (cond ((null? x) 0)
                                         ((not (pair? x)) 1)
                                         (else (count-leaves x)))) t)))

(count-leaves (list (list 1 3 4) (list 2 6)))