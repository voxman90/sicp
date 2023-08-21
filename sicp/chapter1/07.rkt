#lang sicp

#|
  Упражнение 1.7

  Проверка good-enough?, которую мы использовали для вычисления квадратных корней, будет
  довольно неэффективна для поиска квадратных корней от очень маленьких чисел. Кроме того,
  в настоящих компьютерах арифметические операции почти всегда вычисляются с ограниченной
  точностью. Поэтому наш тест оказывается неадекватным и для очень больших чисел.
  Альтернативный подход к реализации good-enough? состоит в том, чтобы следить, как от одной
  итерации к другой изменяется guess, и остановиться, когда изменение оказывается небольшой
  долей значения приближения. Разработайте процедуру вычисления квадратного корня square-root,
  которая использует такой вариант проверки на завершение. Верно ли, что на больших и маленьких
  числах она работает лучше?
|#

(#%require rackunit)

(define (average x y)
  (/ (+ x y) 2))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (get-part x percent)
  (* (/ x 100)
     percent))

(define (good-enough? guess improved-guess)
  (< (abs (- improved-guess guess))
     (get-part guess 0.001)))

(define (sqrt-iter guess improved-guess x)
  (if (good-enough? guess improved-guess)
      improved-guess
      (sqrt-iter improved-guess (improve improved-guess x) x)))

(define (square-root-alt x)
  (sqrt-iter 1 (improve 1 x) x))

(check-equal? (round (* 1000 (square-root-alt 4.0))) 2000.0)
(check-equal? (round (* 1000 (square-root-alt 100.0))) 10000.0)
(check-equal? (round (* 1000 (square-root-alt 1000000.0))) 1000000.0)
(check-equal? (round (* 1000 (square-root-alt 0.04))) 200.0)
(check-equal? (round (* 1000 (square-root-alt 999999999991111.0))) 31622776602.0)
(check-equal? (round (* 1000 (square-root-alt 10000000000000000.0))) 100000000000.0)

#|
  Версия sqrt-iter с менее продвинутой проверкой приближения:
|#

(define (square x)
  (* x x))

(define (square-root x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x))
       0.0001))

  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

  (sqrt-iter 1 x))

(check-equal? (round (* 1000 (square-root 4.0))) 2000.0)
(check-equal? (round (* 1000 (square-root 100.0))) 10000.0)
(check-equal? (round (* 1000 (square-root 1000000.0))) 1000000.0)
(check-equal? (round (* 1000 (square-root 0.04))) 200.0)
(check-equal? (round (* 1000 (square-root 10000000000000000.0))) 100000000000.0)

#|
(check-equal? (round (* 1000 (square-root 999999999991111.0))) 31622776602.0)
|#

#|
  Сравнение и вывод результатов работы двух процедур:
|#

(define (sign x)
  (if (< x 0)
      -1
      (if (> x 0)
          1
          0)))

(define (abs-diff x y)
  (abs (- (square y) x)))

(define (displayln str)
  (display str)
  (newline))

(define (display-result open-str var close-str result)
  (display open-str)
  (display var)
  (display close-str)
  (displayln result))

(define (who-is-closer x)
  (define diff-alt (abs-diff x (square-root-alt x)))
  (define diff (abs-diff x (square-root x)))
  (case (sign (- diff-alt diff))
        [-1 "square-root-alt"]
        [1 "square-root"]
        [else "equal"]))

(define (display-better-sqrt-approx x)
  (define result-alt (square-root-alt x))
  (define result (square-root x))
  (display-result "(square-root-alt " x ") => " result-alt)
  (display-result "(square-root " x ") => " result)
  (display-result "(who-is-closer " x ") => " (who-is-closer x)))

(display-better-sqrt-approx 1.0)
(display-better-sqrt-approx 4.0)
(display-better-sqrt-approx 16.0)
(display-better-sqrt-approx 100.0)
(display-better-sqrt-approx 1000000.0)
(display-better-sqrt-approx 999999999999999.0)
(display-better-sqrt-approx 0.04)
(display-better-sqrt-approx 0.0001)
(display-better-sqrt-approx 0.00000001)

#|
  Верно, что процедура square-root-alt работает для малых чисел лучше, чем процедура square-root,
  т.к. её критерий проверки приближения изменяется в зависимости от величины приближения, составляя
  от последнего постоянную долю (например, одну тысячную). Критерий проверки square-root статичен
  (например, одна тысячная) и если приближаемое число слишком мало, процедура становится неточной.

  Проверки показывают, что для больших чисел обе процедуры дают хороший результат. Для достаточно
  больших чисел, процедура square-root начинает давать более точные результаты чем square-root-alt.
  Но есть нюанс. Иногда она зацикливается, т.к. приближенное значение из-за неточности вычислений не
  может удовлетворить критерий проверки. Например, для числа 999999999991111.0 square-root вообще не
  выдаст результат (а square-root-alt выдаст).
  Так что процедура square-root-alt лучше и для больших чисел.
|#
