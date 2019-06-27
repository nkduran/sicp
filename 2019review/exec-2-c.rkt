#lang racket

(define (new-map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (new-map proc (cdr items)))))

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

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x) (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

#! sicp 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))


(define mat '((1 3 4) (5 8 7) (2 4 9)))
(define mat2 '((2 5 8) (3 6 9) (1 1 8)))
(define v1 '(1 3 4))
(define v2 '(2 5 8))

(dot-product v1 v2)
(matrix-*-vector  mat v1)
(transpose mat)
(matrix-*-matrix mat mat2)

#! sicp 2.38
(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (fold-iter result rest)
    (if (null? rest)
        result
        (fold-iter (op result (car rest))
                   (cdr rest))))
  (fold-iter initial sequence))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))

#! sicp 2.39
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse-fold-left sequence)
  (fold-left (lambda (already-reversed first) (cons first already-reversed)) '() sequence))

(define (reverse-fold-right sequence)
  (fold-right (lambda (first already-reversed) (append already-reversed (list first))) '() sequence))

(reverse-fold-left (list 1 2 3 4 6))
(reverse-fold-right (list 1 2 3 4 6))

#! =====

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (make-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
          (enumerate-interval 1 n)))

;(make-pairs 10)

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (make-pairs n))))

(prime-sum-pairs 10)

(define (permutations s)
  (if (null? s)
      '()
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
                 s)))

(define (remove item seq)
  (filter (lambda (x) (not (= x item))) seq))

(permutations (list 1 2 3))

#! sicp 2.40
(define (unique-pairs n)
  (filter (lambda (x)
            (> (car x) (cadr x))) (make-pairs n)))

(define (prime-sum-pairs-new n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs-new 10)

#! sicp 2.41

(define (make-triple n)
  (flatmap
   (lambda (p)
     (map (lambda (j) (cons j (reverse p)))
          (enumerate-interval 1 (- (cadr p) 1))))
  (make-pairs n)))

(define (sum-equal-triples n s)
  (filter (lambda (t) (= (accumulate + 0 t) s)) (make-triple n)))

(sum-equal-triples 10 15)

; sicp 2.42

(define empty-board '())
(define (adjoin-position new-row col rest-of-queens)
  (cons (list new-row col) rest-of-queens))

(define (safe? new-pos)
  (let ((queen (car new-pos))
        (queen-row (caar new-pos))
        (queen-col (cadar new-pos))
        (rest-of-board (cdr new-pos)))
    (define (safe-iter board)
      (cond ((null? board) #t)
            ((= queen-row (caar board)) #f)
            ((= queen-col (cadar board)) #f)
            ((= (- queen-row queen-col) (- (caar board) (cadar board))) #f)
            ((= (+ queen-row queen-col) (+ (caar board) (cadar board))) #f)
            (else #t)))
    (safe-iter (cdr new-pos))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;(queens 8)

