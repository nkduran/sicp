#lang racket
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)

(cons x y)

(list x y)

(define xxx (list (list 1 2) (list 3 4)))

xxx

(define (deep-reverse items)
  (cond ((not (pair? items)) items)
        (else (map deep-reverse items)
              (reverse items))))

(deep-reverse '((1 2) (3 4)))

(define (fringe items)
  (define (new-append list1 list2)
    (cond ((null? list2) list1)
          ((and (not (pair? list1)) (not (pair? list2))) (append (list list1) (list list2)))
          ((not (pair? list1)) (append (list list1) list2))
          ((not (pair? list2)) (append list1 (list list2)))
          (else (append list1 list2))))
  (cond ((not (pair? items)) items)
        (else (new-append (fringe (car items))
                    (fringe (cdr items))))))

(fringe '((1 2) (3 4)))