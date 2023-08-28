#lang sicp

#|
  Упражнение 1.36

  Измените процедуру fixed-point так, чтобы она печатала последовательность приближений, которые
  порождает, с помощью примитивов newline и display, показанных в упражнении 1.22. Затем найдите
  решение уравнения xˣ = 1000 путем поиска неподвижной точки x → log(1000)/log(x). (Используйте
  встроенную процедуру Scheme log, которая вычисляет натуральные логарифмы.) Посчитайте, сколько
  шагов это занимает при использовании торможения усреднением и без него. (Учтите, что нельзя
  начинать fixed-point со значения 1, поскольку это вызовет деление на log(1) = 0.)
|#

(#%require rackunit)

(define TOLERANCE 0.00001)

(define (fixed-point f first-guess)
  (define counter 0)

  (define (print-guess guess next-guess)
    (set! counter (+ counter 1))
    (map display (list "№" counter " guess: " guess "; next-guess: " next-guess ";\n")))

  (define (close-enough? guess next-guess)
    (< (abs (- guess next-guess)) TOLERANCE))

  (define (try guess)
    (let ((next-guess (f guess)))
      (print-guess guess next-guess)
      (if (close-enough? guess next-guess)
          next-guess
          (try next-guess))))

    (try first-guess))

(define (x^x x)
  (/ (log 1000)
     (log x)))

(define (x^x-average-damped x)
  (/ (+ x (/ (log 1000)
             (log x)))
     2))

(check-equal? (round (* 10000 (fixed-point x^x 2.0))) 45555.0)
(check-equal? (round (* 10000 (fixed-point x^x-average-damped 2.0))) 45555.0)

#|
  Без усреднения вычисление корня уравнения xˣ = 1000 заняло 34 шага, а с усреднением всего 9.
|#

(check-equal? (round (* 100 (fixed-point cos 1.0))) 74.0)
(check-equal? (round (* 100 (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0))) 126.0)
