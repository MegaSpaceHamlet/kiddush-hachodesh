(module kiddush-hachodesh racket
  (require "functions.rkt")
  (provide (all-defined-out))
  (provide (all-from-out "functions.rkt"))
  (define days-of-week (list "Shabbos" "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday"))

  (define (enumerate-months-of-year y)
    (define (enumerator i)
      (if (= i 12)
          (if (leap-year? y)
              (cons 12 nil)
              nil)
          (cons i (enumerator (+ i 1)))))
    (enumerator 0))

  ; Intervals
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
  (define max-days-in-week 7)
  (define no-time (make-interval 0 0 0))
  (define chalokim-to-minute 18)

  (define (past-chatzos? molad) (>= (shaos molad) 18))

  (define (make-interval-addition max-days)
    (lambda (a b . w)
      (define (do-add-interval a b)
        (let ((total-chalokim (+ (chalokim a) (chalokim b)))
              (total-shaos (+ (shaos a) (shaos b)))
              (total-days (+ (days a) (days b))))
          (let ((actual-shaos (if (>= total-chalokim max-chalokim)
                                  (+ 1 total-shaos)
                                  total-shaos)))
            (let ((actual-days (if (>= actual-shaos max-shaos)
                                   (+ 1 total-days)
                                   total-days)))
              (make-interval (if (> max-days 0)
                                 (% actual-days max-days)
                                 actual-days)
                             (% actual-shaos max-shaos)
                             (% total-chalokim max-chalokim))))))
      (accumulate-list do-add-interval (do-add-interval a b) w)))

  (define (make-interval-multiplication max-days)
    (lambda (i n)
      (if (<= n 0)
          no-time
          (let ((total-chalokim (* (chalokim i) n)))
            (let ((total-shaos (if (>= total-chalokim max-chalokim)
                                   (+ (floor (/ total-chalokim max-chalokim)) (* (shaos i) n))
                                   (* (shaos i) n))))
              (let ((total-days (if (>= total-shaos max-shaos)
                                    (+ (floor (/ total-shaos max-shaos)) (* (days i) n))
                                    (* (days i) n))))
                (make-interval (if (> max-days 0)
                                   (% total-days max-days)
                                   total-days)
                               (% total-shaos max-shaos)
                               (% total-chalokim max-chalokim))))))))
    
    

  (define add-interval (make-interval-addition max-days-in-week))

  (define mul-interval (make-interval-multiplication max-days-in-week))

  (define (lt-interval a b)
    (or (< (days a) (days b))
        (and (= (days a) (days b))
             (or (< (shaos a) (shaos b))
                 (and (= (shaos a) (shaos b))
                      (< (chalokim a) (chalokim b)))))))

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
      (display (list-ref days-of-week day))
      (display " ")
      (display hour)
      (display ":")
      (cond ((< minutes 10) (display "0")))
      (display minutes)
      (display am-pm)
      (cond ((> chalokim 0)
             (display ", ")
             (display chalokim)
             (display " chalokim")))))

  (define (convert-interval-to-chalokim i)
    (+ (* (days i) max-shaos max-chalokim)
       (* (shaos i) max-chalokim)
       (chalokim i)))



  ; Get Molad                           
  (define first-molad (make-interval 2 5 204))
  (define month-remainder (make-interval 1 12 793))
  (define machzor-remainder (make-interval 2 16 595)) ; Lunar Movement per Machzor
  (define reg-year-remainder (make-interval 4 8 876))
  (define leap-year-remainder (make-interval 5 21 589))

  (define (get-molad m y)
    (let ((machzors (get-machzors y))
          (remaining-years (% y machzor)))
      (add-interval first-molad (mul-interval machzor-remainder machzors)
                    (calculate-remaining-years (- remaining-years 1)) (mul-interval month-remainder m))))

  (define (calculate-remaining-years yr)
    (cond ((= yr 0) no-time)
          ((leap-year? yr) (add-interval leap-year-remainder (calculate-remaining-years (- yr 1))))
          (else (add-interval reg-year-remainder (calculate-remaining-years (- yr 1))))))



  ; Chapter 7 - Rosh Hashana

  (define (get-rh y)
    (define (later-in-same-day a b) (and (= (days a) (days b)) (>= (shaos a) (shaos b)) (>= (chalokim a) (chalokim b))))
    (define (test-adu d)
      (define (is-adu? d) (or (= d 1) (= d 4) (= d 6)))
      (% (if (is-adu? d) (+ 1 d) d) max-days-in-week))
    (let ((year-molad (get-molad 0 y))
          (three-nine (make-interval 3 9 204))
          (two-fifteen (make-interval 2 15 589)))
      (cond
        ((past-chatzos? year-molad) (test-adu (+ 1 (days year-molad))))
        ((or
          (and (not leap-year?) (later-in-same-day year-molad three-nine))
          (and (leap-year? (- y 1)) (later-in-same-day year-molad two-fifteen))
          )
         (test-adu (+ (days year-molad) 1)))
        (else (test-adu (days year-molad))))))

  ; Chapter 8 - Length of Months
  (define LACKING 0) ; chasser
  (define BALANCED 1) ; mesudar
  (define FULL 2) ; malleh

  (define (year-type y)
    (define (days-between d1 d2)
      (define (do-test d n)
        (if (= (% d max-days-in-week) d2)
            n
            (do-test (+ d 1) (+ n 1))))
      (let ((diff (do-test (+ d1 1) 0)))
        (if (= diff 0)
            5
            diff)))
    (let ((rh1 (get-rh y))
          (rh2 (get-rh (+ y 1))))
      (let ((diff (days-between rh1 rh2)))
        (cond
          ((= rh1 3) 1)
          ((leap-year? y)
           (cond ((= diff 4) LACKING)
                 ((= diff 5) BALANCED)
                 ((= diff 6) FULL)))
          (else (cond ((= diff 2) LACKING)
                      ((= diff 3) BALANCED)
                      ((= diff 4) FULL)))))))


  (define (is-month-full? m y)
    (let ((year (year-type y)))
      (cond ((= m 0) #t); month is Tishrei - always full
            ((= m 1) ; month is cheshvan
             (or (= year FULL) (= year BALANCED))) ; year must be "ordered" or "full"
            ((= m 2) (= year BALANCED)) ; month is Kislev. year must be "full"
            ((= m 3) #f); month is Teves - always lacking
            ((= m 5) (leap-year? y)) ; Month is Adar. If leap year, it is adar rishon and full.
            ((= m 6) (not (leap-year? y))) ; Month is Nissan or Adar Sheni. If leap year, it is empty
            ((>= m 7)
             (if (leap-year? y)
                 (odd? m)
                 (even? m))))))

  ; Rosh Chodesh

  (define (make-rc f s)
    (cons f s))

  (define (first-day rc)
    (car rc))

  (define (second-day rc)
    (cdr rc))

  (define (get-rc m y)
    (cond ((= m 0)
           (let ((rc (get-rh y)))
             (make-rc rc
                      (% (+ 1 rc) max-days-in-week))))
          (else
           (let ((molad (get-molad m y))
                 (prev-month-full? (is-month-full? (- m 1) y)))
             (let ((molad-past-chatzos? (past-chatzos? molad)))
               (cond ((and molad-past-chatzos? prev-month-full?)
                      (make-rc (% (+ 1 (days molad)) max-days-in-week)
                               (% (+ 2 (days molad)) max-days-in-week)))
                     (molad-past-chatzos?
                      (make-rc (% (+ 1 (days molad)) max-days-in-week)
                               nil))
                     (prev-month-full?
                      (make-rc (days molad)
                               (% (+ 1 (days molad)) max-days-in-week)))
                     (else
                      (make-rc (days molad)
                               nil))))))))
                 
                 
                           
               

  ; Chapter 9 - Tekufah: Shmuel's approach. Solar year is 365:6:0

  (define solar-lunar-machzor-diff (make-interval 0 1 485))
  (define tekufah-difference (make-interval 91 7 540))
  (define tekufos (list "Nissan" "Tammuz" "Tishrei" "Teves"))
  (define first-tekufah (make-interval 7 9 642)) ; really it's -7 -9 -642 from first molad nissan.
  ; Since the program subtracts this interval from the accumulation,
  ; I defined it as a regular interval.
  ; Also, it is a regular interval. It just doesn't represent a day of the week.
  (define tekufah-year-remainder (make-interval 10 21 204))
  (define lunar-cycle (make-interval 29 12 793))

  (define (sub-interval b a)
    (let ((diff-chalokim (- (chalokim b) (chalokim a))))
      (let ((diff-shaos (- (shaos b) (shaos a) (if (< diff-chalokim 0) 1 0))))
        (let ((diff-days (- (days b) (days a) (if (< diff-shaos 0) 1 0))))
          (make-interval diff-days
                         (% (if (negative? diff-shaos) (- max-shaos (abs diff-shaos)) diff-shaos) max-shaos)
                         (% (if (negative? diff-chalokim) (- max-chalokim (abs diff-chalokim)) diff-chalokim) max-chalokim))))))



  (define (subtract-all-lunar-cycles i)
    (if (lt-interval i lunar-cycle)
        i
        (subtract-all-lunar-cycles (sub-interval i lunar-cycle))))

  (define (get-tekufas-nissan y)
    (let ((total-cycle-accumulation (mul-tekufah solar-lunar-machzor-diff (get-machzors y))))
      (let ((remaining-month  (subtract-all-lunar-cycles
                               (sub-interval
                                (add-tekufah
                                 total-cycle-accumulation
                                 (mul-tekufah tekufah-year-remainder (- (% y machzor) 1)))
                                first-tekufah))))
        (let ((m (if (and (leap-year? y)
                          (lt-interval remaining-month total-cycle-accumulation))
                     7
                     6)))
          (let ((molad (get-molad m y)))
            (let ((rc (get-rc m y)))
              (add-tekufah
               remaining-month
               (make-interval (if (past-chatzos? molad)
                                  0
                                  1)
                              (shaos molad) (chalokim molad)))
              ))))))

  (define (get-tekufah t y)
    (let ((tekufas-nissan (get-tekufas-nissan y)))
      (let ((interval (add-tekufah
                       (get-tekufas-nissan y)
                       (mul-tekufah tekufah-difference t)))
            (nissan (if (leap-year? y) 7 6)))
        (make-interval (day (add-hebrew-date
                             (make-hebrew-date (days tekufas-nissan) nissan y)
                             (- (days interval) (days tekufas-nissan))))
                       (shaos interval)
                       (chalokim interval)))))
      

  (define add-tekufah (make-interval-addition 0)) ; Creates addition with no max for days

  (define mul-tekufah (make-interval-multiplication 0)) ; Same, for multiplication
                                                 
                 
                                                 
                       
  ; Hebrew Dates
  (define (make-hebrew-date d m y)
    (list d m y))

  (define (day hd)
    (car hd))

  (define (month hd)
    (cadr hd))

  (define (year hd)
    (caddr hd))

  (define machzor 19)

  (define (leap-year? y)
    (if (> y machzor)
        (leap-year? (% y machzor))
        (or (= y 3) (= y 6) (= y 8) (= y 11) (= y 14) (= y 17) (= y 19))))

  (define (get-machzors y)
    (floor (/ y machzor)))

  (define (add-hebrew-date d n)
    (define (shift-month d)
      (let ((max-months (if (leap-year? (year d)) 12 11))
            (next-month (+ 1 (month d))))
        (make-hebrew-date 1
                          (if (> next-month max-months)
                              1
                              next-month)
                          (if (> next-month max-months)
                              (+ (year d) 1)
                              (year d)))))
    (define (add-one d)
      (let ((next-day (+ 1 (day d)))
            (full-month? (is-month-full? (month d) (year d)))
            (is-leap-year? (leap-year? (year d))))
        (let ((max-months (if is-leap-year? 12 11))
              (max-days-of-month (if full-month? 30 29))) ; starting from index 0
          (cond ((> next-day max-days-of-month)
                 (shift-month d))
                (else (make-hebrew-date next-day
                                        (month d)
                                        (year d)))))))
    (if (= n 0)
        d
        (add-hebrew-date (add-one d) (- n 1))))
  )
