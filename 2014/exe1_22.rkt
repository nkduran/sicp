#lang racket

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square a)
  (* a a))

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (report-next-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n start-time)
  (if (prime? n)
      (report-next-prime n (- (current-milliseconds) start-time))
      (search-for-primes (next n) start-time)))

(define (next n)
  (+ n 1))

(search-for-primes (next 100000000) (current-milliseconds))