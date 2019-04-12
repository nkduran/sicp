#lang racket
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define new-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(square-tree new-tree)

(define (new-square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (new-square-tree sub-tree)
             (* sub-