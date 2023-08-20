#lang sicp

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
     (get-part guess 1)))

(define (cube-iter guess improved-guess x)
  (if (good-enough? guess improved-guess)
      improved-guess
      (cube-iter improved-guess (improve improved-guess x) x)))

(define (solution x)
  (exact->inexact (cube-iter 1 (improve 1 x) x)))

(check-equal? (< (- (solution 8) 2) 0.001) #t)
(check-equal? (< (- (solution 27) 3) 0.001) #t)
(check-equal? (< (- (solution 216) 6) 0.001) #t)
