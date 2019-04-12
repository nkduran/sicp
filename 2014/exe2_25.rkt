#lang racket

(define list1 (list 1 3 '(5 7) 9))

(define list2 (list '(7)))

(define list3 (list 1 '(2 (3 (4 (5 (6 7)))))))

(car (cdr (car (cdr (cdr list1)))))

(car (car list2))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list3))))))))))))