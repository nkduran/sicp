#lang racket

(define nil '())

(define cons-list-2 (cons 1 (cons 2 (cons 3 (cons 4 nil)))))

(define cons-list (list 1 2 3 4 5 6))
 
cons-list

(car cons-list)

(cdr cons-list)

(cons 10 cons-list)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(list-ref cons-list 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(length cons-list)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(append cons-list cons-list)

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4) 10)

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -10 2.4 -11.6 17))

(define (new-scale-list items factor)
  (map (lambda (x) (* x factor)) items))

(new-scale-list cons-list 10)
