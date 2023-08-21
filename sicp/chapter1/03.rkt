#lang sicp

#|
  Упражнение 1.3

  Определите процедуру solution, которая принимает в качестве аргументов три числа и возвращает 
  сумму квадратов двух больших из них.
|#

(#%require rackunit)

(define (<= x y)
  (not (> x y)))

(define (square x)
  (* x x))

(define (sum_of_square x y)
  (+ (square x) (square y)))

(define (max x y)
  (if (<= x y)
      y
      x))

(define (min x y)
  (if (<= x y)
    x
    y))

(define (sum_of_max_square x y z)
  (sum_of_square (max x y) (max (min x y) z)))

(define solution sum_of_max_square)

(check-equal? (solution 1 2 3) 13)
(check-equal? (solution 4 2 3) 25)
(check-equal? (solution 0 0 0) 0)
(check-equal? (solution 1 0 1) 2)
(check-equal? (solution 2 3 2) 13)
