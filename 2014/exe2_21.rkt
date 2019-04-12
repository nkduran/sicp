#lang racket

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-list list1)
  (map (lambda (x) (* x x)) list1))

(square-list (list 1 2 3 4))

(define (square x)
  (* x x))

(define (append-value list1 value)
  (if (null? list1)
      (list value)
      (cons (car list1) (append-value (cdr list1) value))))

(define (new-square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append-value answer
                    (square (car things))))))
  (iter items '()))

(new-square-list (list 1 2 3 4))
