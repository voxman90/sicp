#lang sicp

#|
  Упражнение 1.35

  Покажите, что золотое сечение φ (раздел 1.2.2) есть неподвижная точка трансформации x → 1 + 1/x,
  и используйте этот факт для вычисления φ с помощью процедуры fixed-point.
|#

#|
    φ = (1 + √5)/2,

    1 + 1/φ = 1 + 1/((1 + √5)/2) = 1 + 2/(1 + √5) = (1 + √5)/(1 + √5) + 2/(1 + √5) =

    = (1 + √5 + 2)/(1 + √5) = (2 + 2√5 + 4)/2(1 + √5) = (1 + √5 + √5 + 5)/2(1 + √5) =

    = (1 + √5)(1 + √5)/2(1 + √5) = (1 + √5)/2 = ф

  Таким образом, ф неподвижная точка трансформации x → 1 + 1/x. Реализуем процедуру её нахождения
  с помощью  fixed-point:
|#

(#%require rackunit)

(define TOLERANCE 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? guess next-guess)
    (< (abs (- guess next-guess)) TOLERANCE))

  (define (try guess)
    (let ((next-guess (f guess)))
      (if (close-enough? guess next-guess)
          next-guess
          (try next-guess))))

    (try first-guess))

(define (golden-ratio x)
  (+ 1 (/ 1 x)))

(define (golden-ratio-average-damping x)
  (/ (+ 1 x (/ 1 x)) 2))

(define solution golden-ratio)

(check-equal? (round (* 10000 (fixed-point solution 1.0))) 16180.0)
(check-equal? (round (* 10000 (fixed-point golden-ratio-average-damping 1.0))) 16180.0)
