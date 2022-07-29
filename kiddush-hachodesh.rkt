#lang sicp
(#%require "functions.rkt")
(#%provide (all-defined))

(define % modulo)

(define (make-interval d s c)
  (list d s c))

(define (days i)
  (car i))

(define (shaos i)
  (cadr i))

(define (chalokim i)
  (caddr i))

(define max-chalokim 1080)
(define max-shaos 24)
(define max-days 7)
(define machzor 19)
(define no-time (make-interval 0 0 0))
(define chalokim-to-minute 18)

(define (add a b)
  (let ((total-chalokim (+ (chalokim a) (chalokim b)))
        (total-shaos (+ (shaos a) (shaos b)))
        (total-days (+ (days a) (days b))))
    (let ((actual-shaos (if (>= total-chalokim max-chalokim)
                            (+ 1 total-shaos)
                            total-shaos)))
      (let ((actual-days (if (>= actual-shaos max-shaos)
                             (+ 1 total-days)
                             total-days)))
        (make-interval (% actual-days max-days)
                       (% actual-shaos max-shaos)
                       (% total-chalokim max-chalokim))))))

(define (mul-rec i n)
  (if (<= n 0)
      no-time
      (if (= n 1)
          i
          (add i (mul i (- n 1))))))

(define (mul i n)
  (if (<= n 0)
      no-time
      (let ((total-chalokim (* (chalokim i) n)))
         (let ((total-shaos (if (>= total-chalokim max-chalokim)
                                (+ (floor (/ total-chalokim max-chalokim)) (* (shaos i) n))
                                (* (shaos i) n))))
           (let ((total-days (if (>= total-shaos max-shaos)
                                 (+ (floor (/ total-shaos max-shaos)) (* (days i) n))
                                 (* (days i) n))))
             (make-interval (% total-days max-days)
                            (% total-shaos max-shaos)
                            (% total-chalokim max-chalokim)))))))

(define (leap-year? y)
  (if (> y machzor)
      (leap-year? (% y machzor))
      (or (= y 3) (= y 6) (= y 8) (= y 11) (= y 14) (= y 17) (= y 19))))

(define (calculate-remaining-years yr)
    (cond ((= yr 0) no-time)
          ((leap-year? yr) (add leap-year-remainder (calculate-remaining-years (- yr 1))))
          (else (add reg-year-remainder (calculate-remaining-years (- yr 1))))))

(define day-names (list "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Shabbos"))
(define (pretty-print-interval i)
  (let ((minutes (floor (/ (chalokim i) chalokim-to-minute)))
        (chalokim (% (chalokim i) chalokim-to-minute))
        (day (if (<= (shaos i) 6)
                 (- (days i) 1)
                 (days i)))
        (am-pm (if (or (<= (shaos i) 6) (>= (shaos i) 18))
                   "PM"
                   "AM"))
        (hour (cond ((<= (shaos i) 6) (+ (shaos i) 6))
                    ((and (> (shaos i) 6) (<= (shaos i) 18))
                     (- (shaos i) 6))
                    ((> (shaos i) 18) (- (shaos i) 18)))))
    (display (list-ref day-names day))
    (display " ")
    (display hour)
    (display ":")
    (display minutes)
    (display am-pm)
    (cond ((> chalokim 0)
           (display ", ")
           (display chalokim)
           (display " chalokim")))))
        
               
                           
(define first-molad (make-interval 2 5 204))
(define month-remainder (make-interval 1 12 793))
(define machzor-remainder (make-interval 2 16 595))
(define reg-year-remainder (make-interval 4 8 876))
(define leap-year-remainder (make-interval 5 21 589))