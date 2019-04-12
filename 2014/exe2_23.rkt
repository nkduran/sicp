#lang racket

(define (for-each proc items)
  (let ((item-cdr (cdr items)))
    (proc (car items))
    (if (null? item-cdr)
        true
        (for-each proc item-cdr))))

(for-each (lambda (x) (newline) (display x))
          (list 1 3 9 11))
      