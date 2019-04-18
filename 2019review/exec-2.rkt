#lang racket

#! sicp 2.2

(define (make-segment startp endp)
  (cons startp endp))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment segment)
  (define x (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2))
  (define y (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2))
  (make-point x y))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define startp1 3.0)
(define endp1 6.0)
(define startp2 10.0)
(define endp2 18.0)

(define l1 (make-segment (make-point startp1 endp1) (make-point startp2 endp2)))
(print-point (midpoint-segment l1))

#! sicp 2.3
(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (make-rect lt-point rb-point)
  (cons lt-point rb-point))

(define (lt-point rect)
  (car rect))

(define (rb-point rect)
  (cdr rect))

(define (width rect)
  (abs (- (x-point (lt-point rect)) (x-point (rb-point rect)))))

(define (height rect)
  (abs (- (y-point (lt-point rect)) (y-point (rb-point rect)))))

(define (perimeter rect)
  (* 2 (+ (width rect) (height rect))))

(define (area rect)
  (* (width rect) (height rect)))

(define one-rect (make-rect (make-point 1 10) (make-point 20 5)))
(perimeter one-rect)
(area one-rect)

#! sicp 2.17
(define l (list 23 72 149 34))

(define (last-pair list1)
  (if (null? (cdr list1))
      list1
      (last-pair (cdr list1))))

(last-pair l)

#! sicp 2.18
(define (reverse list1)
  (define (reverse-iter l result)
    (if (null? l)
        result
        (reverse-iter (cdr l) (cons (car l) result))))
  (reverse-iter list1 '()))

(reverse l)

#! sicp 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (define (no-more? l)
    (null? l))
  (define (first-denomination l)
    (car l))
  (define (except-first-denomination l)
    (cdr l))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(cc 100 us-coins)

#! sicp 2.20
(define (same-parity n . l)
  (define (divided? m n)
    (= (remainder m n) 0))
  (define (same-parity-iter number list1 result)
    (define item (if (pair? list1)
                     (car list1)
                     list1))
    (if (null? list1)
        result
        (if (divided? (- item number) 2)
            (same-parity-iter number (cdr list1) (cons item result))
            (same-parity-iter number (cdr list1) result))))
  (cons n (reverse (same-parity-iter n l '()))))

(same-parity 1 2 3 4 5 6 7)

#! sicp 2.21
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (map proc (cdr items)))))

(define (new-square x) (* x x))

(define (square-list items)
  (if (null? items)
      '()
      (cons (new-square (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map new-square items))

(square-list (list 2 4 6))
(square-list2 (list 3 9 18))

#! sicp 2.23
(define (for-each proc items)
  (proc (car items))
  (if (null? (cdr items))
      true
      (for-each proc (cdr items))))

(for-each (lambda (x) (newline) (display x)) (list 44 22 323))

#! sicp 2.25
(car (cdaddr '(1 3 (5 7) 9)))
(caar '((7)))
(cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))

#! sicp 2.27
(define newlist '((1 2) (3 4) (5 6 7 8)))

(define (deep-reverse list1)
  (define (deep-reverse-car item)
    (if (not (pair? item))
        item
        (deep-reverse item)))
  (define (deep-reverse-iter l result)
    (if (null? l)
        result
        (deep-reverse-iter (cdr l) (cons (deep-reverse-car (car l)) result))))
  (deep-reverse-iter list1 '()))

(reverse newlist)
(deep-reverse newlist)

#! sicp 2.28
(define (fringe list1)
  (define (fringe-iter l result)
    (cond ((null? l) result)
          ((not (pair? l)) (cons l result))
          (else (fringe-iter (car l) (fringe-iter (cdr l) result)))))
  (fringe-iter list1 '()))

(fringe newlist)

#! sicp 2.29
#! a)
(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

#! b)
(define (total-weight mobile)
  (define (branch-weight branch)
    (cond ((null? branch) 0)
          ((not (pair? branch)) branch)
          (else (+ (branch-length branch) (branch-weight (branch-structure branch))))))
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define branch1 (make-branch 8 '()))
(define branch2 (make-branch 7 branch1))
(define branch3 (make-branch 9 '()))
(define branch4 (make-branch 2 branch3))
(define mobile1 (make-mobile branch2 branch4))

(total-weight mobile1)

