#lang racket

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (filter predicate items)
  (cond ((null? items) '())
        ((predicate (car items))
         (cons (car items)
               (filter predicate (cdr items))))
        (else (filter predicate (cdr items)))))

(define (accumulate op initial items)
  (if (null? items)
      initial
      (op (car items)
          (accumulate op initial (cdr items)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

#! sicp 2.30

(define (square-tree list1)
  (cond ((null? list1) '())
        ((not (pair? list1)) (* list1 list1))
        (else (cons (square-tree (car list1)) (square-tree (cdr list1))))))

(define (square-tree2 list1)
  (map (lambda (sub-list)
         (if (pair? sub-list)
             (square-tree2 sub-list)
             (* sub-list sub-list)))
       list1))

(define l '(1 (2 3) (4 (5 6) 7) 8) )

(square-tree l)
(square-tree2 l)

#! sicp 2.31
(define (tree-map proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

(define (square x)
  (* x x))

(define (square-tree3 tree)
  (tree-map square tree))

(square-tree3 l)

#! sicp 2.32
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))


(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (ss) (cons (car s) ss)) rest)))))

(define set1 '(1 2 3))
(subsets set1)

#! sicp 2.33
(define (new-map p items)
  (accumulate (lambda (x y) (cons (p x) y)) '() items))

(new-map (lambda (x) (* x x)) (list 1 2 3 5))

(define (new-append seq1 seq2)
  (accumulate cons seq2 seq1))

(new-append (list 1 3 5) (list 3 6 9))

(define (new-length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

(new-length '(1 3 5 6 7))