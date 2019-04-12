#lang racket

(define (pascal-trangle col row)
  (cond ((= col 1) 1)
        ((= row 1) 1)
        (else (+ (pascal-trangle (- col 1) row) (pascal-trangle col (- row 1))))))

(pascal-trangle 4 3)