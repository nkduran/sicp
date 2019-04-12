#lang racket

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (square x)
  (* x x))

(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1)) f)))

(define (start-time-record)
  (define start-time (current-milliseconds))
  (display " *** starttime:")
  (display start-time)
  (display " *** ")
  (newline)
  start-time)

(define (get-time-cost start-time)
  (newline)
  (display " *** time cost:")
  (display (- (current-milliseconds) start-time))
  (display " *** ")
  (newline))

(define (get-func-run-time f x)
  (define start-time (start-time-record))
  (display (f x))
  (get-time-cost start-time))

(get-func-run-time (repeated square 18) 5)

(define (new-repeated f n)
  (cond ((= n 1) f)
        ((even? n)
         (repeated (compose f f) (/ n 2)))
        (else (compose (repeated f (- n 1)) f))))

(get-func-run-time (new-repeated square 18) 5)

