(module functions racket
  (provide (all-defined-out))

  (define % modulo)
  (define nil '())

  (define (list-ref l n)
    (if (= n 0)
        (car l)
        (list-ref (cdr l) (- n 1))))

  (define (accumulate-list comb base l)
    (if (null? l)
        base
        (accumulate-list comb (comb base (car l)) (cdr l))))

  (define (enumerate low high)
    (if (> low high)
        nil
        (cons low (enumerate (+ low 1) high)))))