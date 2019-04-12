#lang racket

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))


(define (product-of-square-of-odd-elements sequence)
  (accumulate *
              1
              (map (lambda (x) (* x x))
                   (filter odd? sequence))))

(product-of-square-of-odd-elements (list 1 2 3 4 5))

(define (new-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(new-map (lambda (x) (* x x)) (list 1 4 5 9 11))

(define (new-append seq1 seq2)
  (accumulate cons seq2 seq1))

(new-append (list 1 3 5) (list 2 4 6))

(define (new-length sequence)
  (accumulate (lambda (x y) (+ (if (null? x)
                                    0
                                    1)
                               y))
                               0 sequence))

(new-length (list 1 3 5 6 7 9))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))