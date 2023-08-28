#lang sicp

#|
  Упражнение 1.40

  Определите процедуру cubic, которую можно было бы использовать совместно с процедурой
  newtons-method в выражениях вида:

    (newtons-method (cubic a b c) 1)

  для приближенного вычисления нулей кубических уравнений x³ + ax² + bx + c.
|#

(#%require rackunit)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? guess next-guess)
    (< (abs (- next-guess guess)) tolerance))

  (define (try guess)
    (let ((next-guess (f guess)))
      (if (close-enough? guess next-guess)
          next-guess
          (try next-guess))))

  (try first-guess))

(define dx 0.00001)

(define (derive f)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x))
       dx)))

(define (newtons-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((derive g) x)))))

(define (newtons-method g first-guess)
  (fixed-point (newtons-transform g) first-guess))

(define (square x)
  (* x x))

(define (cube x)
  (* x (square x)))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(check-equal? (round (newtons-method (cubic 0 0 0) 1)) 0.0)
(check-equal? (round (newtons-method (cubic 0 0 (- 27)) 10)) 3.0)
(check-equal? (round (newtons-method (cubic 0 0 (- 81)) 10)) 4.0)
(check-equal? (round (newtons-method (cubic 2 8 (- 32)) 10)) 2.0)
