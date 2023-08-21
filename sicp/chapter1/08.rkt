#lang sicp

#|
  Упражнение 1.8

  Метод Ньютона для кубических корней основан на том, что если y является приближением
  к кубическому корню из x, то мы можем получить лучшее приближение по формуле:

      x/y^2 + 2y
     ------------
          3

  С помощью этой формулы напишите процедуру вычисления кубического корня cube-root, подобную
  процедуре для квадратного корня. (В разделе 1.3.4 мы увидим, что можно реализовать общий
  метод Ньютона как абстракцию этих процедур для квадратного и кубического корня.)
|#

(#%require rackunit)

(define (square x)
  (* x x))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (get-part x percent)
  (* (/ x 100)
     percent))

(define (good-enough? guess improved-guess)
  (< (abs (- improved-guess guess))
     (get-part guess 0.001)))

(define (cube-iter guess improved-guess x)
  (if (good-enough? guess improved-guess)
      improved-guess
      (cube-iter improved-guess (improve improved-guess x) x)))

(define (cube-root x)
  (cube-iter 1 (improve 1 x) x))

(check-equal? (round (* 1000 (cube-root 8.0))) 2000.0)
(check-equal? (round (* 1000 (cube-root 1000.0))) 10000.0)
(check-equal? (round (* 1000 (cube-root 1000000000.0))) 1000000.0)
(check-equal? (round (* 1000 (cube-root 0.008))) 200.0)
