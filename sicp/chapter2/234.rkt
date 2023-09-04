#lang sicp

#|
  Упражнение 2.34

  Вычисление многочлена с переменной x при данном значении x можно сформулировать в виде накопления.
  Мы вычисляем многочлен

    aₙxⁿ + aₙ₋₁xⁿ⁻¹ + ... + a₁x + a₀

  по известному алгоритму, называемому схема Горнера (Horner’s rule), которое переписывает формулу в
  виде

    (...(aₙx + aₙ₋₁)x + ... + a₁)x + a₀)

  Другими словами, мы начинаем с aₙ, умножаем его на x, и так далее, пока не достигнем a₀. Заполните
  пропуски в следующей заготовке так, чтобы получить процедуру, которая вычисляет многочлены по схеме
  Горнера. Предполагается, что коэффициенты многочлена представлены в виде последовательности, от a₀
  до aₙ.

    (define (horner-eval x coefficient-sequence)
      (accumulate (lambda (this-coeff higher-terms) <??>)
                  0
                  coefficient-sequence))

  Например, чтобы вычислить 1 + 3x + 5x³ + x⁵ в точке x = 2, нужно ввести

    (horner-eval 2 (list 1 3 0 5 0 1))
|#

(#%require rackunit)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
      (accumulate (lambda (this-coeff higher-terms)
                    (+ this-coeff (* x higher-terms)))
                  0
                  coefficient-sequence))

#|
  f(x) = 1 + 3x + 5x³ + x⁵, f(2) = 79
  f(x) = x² - 100, f(3) = -91
  f(x) = x² + 3x + 2, f(-1) = 0
  f(x) = 7, f(100) = 7
|#

(check-equal? (horner-eval 2 (list 1 3 0 5 0 1)) 79)
(check-equal? (horner-eval 3 (list -100 0 1)) -91)
(check-equal? (horner-eval -1 (list 2 3 1)) 0)
(check-equal? (horner-eval 100 (list 7)) 7)
