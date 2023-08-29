#lang sicp

#|
  Упражнение 2.0

  Определите улучшенную версию make-rat, которая принимала бы как положительные, так и отрицательные
  аргументы. Make-rat должна нормализовывать знак так, чтобы в случае, если рациональное число
  положительно, то и его числитель, и знаменатель были бы положительны, а если оно отрицательно, то
  чтобы только его числитель был отрицателен.
|#

(#%require rackunit)

(define (sign a)
  (if (< a 0)
      -1
      1))

(define (make-rat numer denom)
  (let ((gcd (gcd numer denom))
        (sign (* (sign numer) (sign denom))))
    (cons (* sign (abs (/ numer gcd)))
          (abs (/ denom gcd)))))

(check-equal? (make-rat 1 2) '(1 . 2))
(check-equal? (make-rat (- 1) 2) '(-1 . 2))
(check-equal? (make-rat (- 1) (- 2)) '(1 . 2))
(check-equal? (make-rat 1 (- 2)) '(-1 . 2))
(check-equal? (make-rat 2 4) '(1 . 2))
