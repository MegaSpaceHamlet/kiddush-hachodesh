#lang racket
(require "kiddush-hachodesh.rkt")
(require rackunit)

(check-eq? 13 (length (enumerate-months-of-year 5782)))
(check-eq? 12 (length (enumerate-months-of-year 5783)))


(check-equal? (convert-interval-to-chalokim lunar-cycle) 765433)

(let ((y 5782)
      (checks (list
               (make-interval 3 5 497)
               (make-interval 4 18 210)
               (make-interval 6 6 1003)
               (make-interval 0 19 716)
               (make-interval 2 8 429)
               (make-interval 3 21 142)
               (make-interval 5 9 935)
               (make-interval 6 22 648)
               (make-interval 1 11 361)
               (make-interval 3 0 74)
               (make-interval 4 12 867)
               (make-interval 6 1 580)
               (make-interval 0 14 293))))     
  (for-each (lambda (m)
              (check-equal? (list-ref checks m) (get-molad m y)))
            (enumerate-months-of-year y)))

(let ([y 5782]
      [checks (list
               (make-rc 3 4)
               (make-rc 5 6)
               (make-rc 6 0)
               (make-rc 1 2)
               (make-rc 2 nil)
               (make-rc 4 5)
               (make-rc 5 6)
               (make-rc 0 nil)
               (make-rc 1 2)
               (make-rc 3 nil)
               (make-rc 4 5)
               (make-rc 6 nil)
               (make-rc 0 1))])
  (for-each (lambda (rc)
              (check-equal? (list-ref checks rc) (get-rc rc y)))
            (enumerate-months-of-year y)))

(let ([years (append (enumerate 4929 4931) (enumerate 5781 5783))]
      [checks (list
               ; 4929
               (list (make-interval 27 0 0)
                     (make-interval 29 7 540)
                     (make-interval 2 15 0)
                     (make-interval 4 22 540))
               ; 4930 - The Rambam's example year
               (list (make-interval 8 6 0)
                     (make-interval 10 13 540)
                     (make-interval 13 21 0)
                     (make-interval 17 4 540))
               ; 4931
               (list (make-interval 18 12 0)
                     (make-interval 20 19 540)
                     (make-interval 24 3 0)
                     (make-interval 27 10 540))
               ; 5781
               (list (make-interval 26 0 0)
                     (make-interval 28 7 540)
                     (make-interval 1 15 0)
                     (make-interval 3 22 540))
               ; 5782
               (list (make-interval 7 6 0)
                     (make-interval 9 13 540)
                     (make-interval 12 21 0)
                     (make-interval 16 4 540))
               ; 5783
               (list (make-interval 18 12 0)
                     (make-interval 20 19 540)
                     (make-interval 24 3 0)
                     (make-interval 28 10 540)))])
  (for-each (lambda (y)
              (for-each (lambda (t)
                          (check-equal? (list-ref (list-ref checks y) t) (get-tekufah t (list-ref years y))))
                        (enumerate 0 3)))
            (enumerate 0 (- (length years) 1))))
