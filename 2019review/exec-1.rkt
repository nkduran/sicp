#lang racket
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

#! sicp 1.16

(define (fast-expt2 b n)
  (define (fast-expt-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter a (square b) (/ n 2)))
          (else (fast-expt-iter (* a b) b (- n 1)))))
  (fast-expt-iter 1 b n))

(fast-expt2 2 6)

#! sicp 1.17
(define (fast-multiply a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (cond ((= b 0) 0)
        ((even? b) (fast-multiply (double a) (halve b)))
        (else (+ a (fast-multiply a (- b 1))))))

(fast-multiply 6 7)

#! sicp 1.18
(define (fast-multiply2 a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (define (fast-multiply-iter product a b)
    (cond ((= b 0) product)
          ((even? b) (fast-multiply-iter product (double a) (halve b)))
          (else (fast-multiply-iter (+ product a) a (- b 1)))))
  (fast-multiply-iter 0 a b))

(fast-multiply2 5 6)

#! sicp 1.19
(define (fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p) (* q q))
                     (+ (* 2 p q) (* q q))
                     (/ count 2)))
          (else (fib-iter (+ (* p a) (* q a) (* q b))
                          (+ (* q a) (* p b))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))

(fib 10)

#! sicp 1.29

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson-sum f a b n)
  (define h (/ (- b a) n))
  (define (simpson-add x)
    (+ (f x) (* (f (+ x h)) 4) (f (+ x (* 2 h)))))
  (define (next x) (+ x (* 2 h)))
  (* (/ h 3) (sum simpson-add a next b)))

(define (cube x) (* x x x))

(simpson-sum cube 0 1.0 100)

#! sicp 1.30

(define (simpson-rule f a b n)
  (define h (/ (- a b) n))
  (define (simpson-add f a)
    (+ (f a) (* (f (+ a h)) 4) (f (+ a (* 2 h)))))
  (define (simpson-iter f a c product)
    (cond ((not (< c n)) product)
           (else (simpson-iter f a (+ c 2) (+ (simpson-add f (+ a (* h c))) product)))))
  (* (simpson-iter f a 0 0) (/ h 3)))

(simpson-rule (lambda (x) (* x x x)) 0 1.0 10000)

#! sicp 1.31

(define (product term a next n)
  (if (= n 0)
      1
      (* (term a)
         (product term (next a) next (- n 1)))))

(define (next x) (+ x 2))
(define (value x) (/ (* (- x 1) (+ x 1)) (* x x)))
(* (product value 3 next 100) 4.0)

(define (product2 n)
  (define (product-iter result x count)
    (if (= count 0)
        result
        (product-iter (* result (value x)) (next x) (- count 1))))
  (* (product-iter 1 3 n) 4.0))
(product2 100)

#! sicp 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate2 combiner null-value term a next b)
  (define (accumulate-iter result a)
    (if (> a b)
        result
        (accumulate-iter (combiner (term a) result) (next a))))
  (accumulate-iter null-value a))

(define (new-sum term a next b)
  (accumulate + 0 term a next b))

(define (new-sum2 term a next b)
  (accumulate2 + 0 term a next b))

(define (new-product term a next b)
  (accumulate * 1 term a next b))

(define (new-product2 term a next b)
  (accumulate2 * 1 term a next b))

(new-sum (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)
(new-sum2 (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)
(new-product (lambda (x) x) 1 (lambda (x) (+ x 1)) 5)
(new-product2 (lambda (x) x) 1 (lambda (x) (+ x 1)) 5)