#lang racket
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(define (make-rat n d)
  (define numer-sign (/ n (abs n)))
  (define denom-sign (/ d (abs d)))
  (let ((g (gcd n d)))
    (cond ((= numer-sign denom-sign) (cons (abs (/ n g)) (abs (/ d g))))
          (else (cons (* (abs (/ n g)) -1) (abs (/ d g)))))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))

(print-rat one-half)

(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))

(print-rat (add-rat (make-rat 2 3) (make-rat -5 6)))

(print-rat (make-rat -5 -6))

(print-rat (make-rat 5 -6))

(print-rat (make-rat -5 6))