#lang racket
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch structure)
  (car structure))

(define (right-branch structure)
  (cdr structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight structure)
  (define (total-weight-iter items weight)
    (if (null? items)
        weight
        (total-weight-iter (branch-structure items) (+ weight (branch-length items)))))
  (total-weight-iter structure 0))

(define new-structure (make-branch 10 (make-branch 18 (make-branch 222 '()))))

(branch-structure new-structure)
(branch-length new-structure)

(total-weight new-structure)
      