#lang sicp
(#%provide (all-defined))

(define % modulo)

(define (make-interval d s c)
  (list d s c))

(define (days i)
  (car i))

(define (shaos i)
  (cadr i))

(define (chalukos i)
  (caddr i))

(define max-chalukos 1080)
(define max-shaos 24)
(define max-days 7)
(define machzor 19)
(define zero (make-interval 0 0 0))

(define (add a b)
  (let ((total-chalukos (+ (chalukos a) (chalukos b)))
        (total-shaos (+ (shaos a) (shaos b)))
        (total-days (+ (days a) (days b))))
    (let ((actual-shaos (if (>= total-chalukos max-chalukos)
                            (+ 1 total-shaos)
                            total-shaos)))
      (let ((actual-days (if (>= actual-shaos max-shaos)
                             (+ 1 total-days)
                             total-days)))
        (make-interval (% actual-days max-days)
                       (% actual-shaos max-shaos)
                       (% total-chalukos max-chalukos))))))

(define (mul-rec i n)
  (if (<= n 0)
      zero
      (if (= n 1)
          i
          (add i (mul i (- n 1))))))

(define (mul i n)
  (if (<= n 0)
      zero
      (let ((total-chalukos (* (chalukos i) n)))
         (let ((total-shaos (if (>= total-chalukos max-chalukos)
                                (+ (floor (/ total-chalukos max-chalukos)) (* (shaos i) n))
                                (* (shaos i) n))))
           (let ((total-days (if (>= total-shaos max-shaos)
                                 (+ (floor (/ total-shaos max-shaos)) (* (days i) n))
                                 (* (days i) n))))
             (make-interval (% total-days max-days)
                            (% total-shaos max-shaos)
                            (% total-chalukos max-chalukos)))))))

(define (leap-year? y)
  (if (> y machzor)
      (leap-year? (% y machzor))
      (or (= y 3) (= y 6) (= y 8) (= y 11) (= y 14) (= y 17) (= y 19))))

(define (calculate-remaining-years yr)
    (cond ((= yr 0) zero)
          ((leap-year? yr) (add leap-year-remainder (calculate-remaining-years (- yr 1))))
          (else (add reg-year-remainder (calculate-remaining-years (- yr 1))))))
                           
(define first-molad (make-interval 2 5 204))
(define month-remainder (make-interval 1 12 793))
(define machzor-remainder (make-interval 2 16 595))
(define reg-year-remainder (make-interval 4 8 876))
(define leap-year-remainder (make-interval 5 21 589))