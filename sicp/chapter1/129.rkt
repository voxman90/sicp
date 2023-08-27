#lang sicp

#|
  Упражнение 1.29

  Правило Симпсона — более точный метод численного интегрирования, чем представленный выше. С
  помощью правила Симпсона интеграл функции f между a и b приближенно вычисляется в виде:

    [y₀ + 4y₁ + 2y₂ + 4y₃ + 2y₄ + . . . + 2yₙ₋₂ + 4yₙ₋₁ + yₙ]*h/3

  где h = (b − a)/n, для какого - то четного целого числа n, а yₖ = f(a + kh). (Увеличение n
  повышает точность приближенного вычисления.) Определите процедуру simpson, которая принимает
  в качестве аргументов f, a, b и n, и возвращает значение интеграла, вычисленное по правилу
  Симпсона. С помощью этой процедуры проинтегрируйте cube между 0 и 1 (с n = 100 и n = 1000)
  и сравните результаты с процедурой integral, приведенной выше.
|#

(#%require rackunit)

(define (cube x) (* x x x))

(define (sum term next i x steps)
  (if (< i steps)
      (+ (term x i)
         (sum term
              next
              (+ i 1)
              (next x)
              steps))
        0))

(define (simpson func a b steps)
  (define height (/ (- b a) steps))

  (define (cy_i x i)
    (define coefficient (- 4.0 (* 2 (if (even? i) 1 0)))) ; even(i) => 2, odd(i) => 4
    (* coefficient (func x)))

  (define (incremented-value a)
    (+ a height))

  (* (/ height 3)
     (+ (func a)
        (sum cy_i
             incremented-value
             1
             (incremented-value a)
             steps)
        (func b))))

(check-equal? (round (* 100 (simpson cube 0 1 100))) 25.0)
(check-equal? (round (* 100 (simpson cube 0 1 1000))) 25.0)
(check-equal? (floor (* 1000 (simpson cube 0 1 100))) 249.0)
(check-equal? (floor (* 1000 (simpson cube 0 1 1000))) 250.0)
