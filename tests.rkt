#lang sicp
(#%require "kiddush-hachodesh.rkt")
(define y 5782)
(define (test-molad)
  (for-each (lambda (m)
              (display m)
              (display " ")
              (display (get-molad m y))
              (newline))
            (list 0 1 2 3 4 5 6 7 8 9 10 11 (if (leap-year? y) 12))))

(test-molad)

(define (test-rc)
  (for-each (lambda (rc)
              (display rc)
              (display " ")
              (display (get-rc rc y))
              (newline))
            (list 0 1 2 3 4 5 6 7 8 9 10 11 (if (leap-year? y) 12))))

(test-rc)

(define (tekufos-for-year y)
  (display y)
  (newline)
(for-each (lambda (t)
            (display (list-ref tekufos t))
            (display " ")
            (display (get-tekufah t y))
            (newline))
          (list 0 1 2 3)))

(for-each tekufos-for-year (list 4929 4930 4931 5781 5782 5783))

(define (test-dates)
  (define d1 (make-hebrew-date 27 11 5782))
  (display "Starting with ")
  (display d1) (newline)
  (for-each (lambda (n) (display (add-hebrew-date d1 n)) (newline))
            (list 0 1 2 3 4 5 6 7 8 9 10)))

;(test-dates)