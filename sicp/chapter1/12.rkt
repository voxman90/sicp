#lang sicp

(#%require rackunit)

(define (dec a)
  (- a 1))

(define (get-Pascal-tringle-coeff row position)
  (if (or (= row position) (= position 1))
      1
      (+ (get-Pascal-tringle-coeff (dec row) (dec position))
         (get-Pascal-tringle-coeff (dec row) position))))

(define (solution n m)
  (get-Pascal-tringle-coeff n m))

(check-equal? (solution 1 1) 1)
(check-equal? (solution 3 2) 2)
(check-equal? (solution 4 3) 3)
(check-equal? (solution 5 2) 4)
(check-equal? (solution 5 3) 6)
(check-equal? (solution 6 3) 10)
