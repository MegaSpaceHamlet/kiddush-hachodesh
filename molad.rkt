#lang sicp
(#%require "kiddush-hachodesh.rkt")

(define (get-molad m y)
  (let ((machzors (floor (/ y machzor)))
        (remaining-years (% y machzor)))
    (add (add first-molad (mul machzor-remainder machzors))
         (add (calculate-remaining-years (- remaining-years 1)) (mul month-remainder m)))))