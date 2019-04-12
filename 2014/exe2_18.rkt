#lang racket

(define (append-value list1 value)
  (if (null? list1)
      (list value)
      (cons (car list1) (append-value (cdr list1) value))))

(append-value (list 1 3 5) 7)

(define (reverse list1)
  (if (null? (cdr list1))
      (list (car list1))
      (append-value (reverse (cdr list1)) (car list1))))

(reverse (list 1 4 9 16 24))