#lang sicp
(#%provide (all-defined))

(define (list-ref l n)
  (if (= n 0)
      (car l)
      (list-ref (cdr l) (- n 1))))

(define (accumulate-list comb base l)
  (if (null? l)
      base
      (accumulate-list comb (comb base (car l)) (cdr l))))