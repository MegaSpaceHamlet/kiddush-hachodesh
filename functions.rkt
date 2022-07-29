#lang sicp
(#%provide (all-defined))

(define (list-ref l n)
  (if (= n 0)
      (car l)
      (list-ref (cdr l) (- n 1))))