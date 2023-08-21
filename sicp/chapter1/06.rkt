#lang sicp

#|
  Упражнение 1.6

  Лиза П. Хакер не понимает, почему if должна быть особой формой. «Почему нельзя просто
  определить ее как обычную процедуру с помощью cond?» — спрашивает она. Лизина подруга
  Ева Лу Атор утверждает, что, разумеется, можно, и определяет новую версию if:

    (define (new-if predicate then-clause else-clause)
      (cond (predicate then-clause)
            (else else-clause)))

  Ева показывает Лизе новую программу:

    (new-if (= 2 3) 0 5) => 5

    (new-if (= 1 1) 0 5) => 0

  Обрадованная Лиза переписывает через new-if программу вычисления квадратного корня:

    (define (sqrt-iter guess x)
      (new-if (good-enough? guess x)
              guess
              (sqrt-iter (improve guess x)
                         x)))

  Что получится, когда Лиза попытается использовать эту процедуру для вычисления квадратных корней?
  Объясните.
|#

#|
  Процедура sqrt-iter зациклится, потому что интерпретатор использует аппликативный порядок
  вычисления. Оператор ветвления if в Scheme устроен таким образом, что откладывает вычисление
  выражений в ветках непосредственно до перехода к ним. Процедура new-if таким достоинством не
  отбладает, а так как содержимое веток передаётся через параметры than-clause и else-clause,
  то интерпретатор осуществит подстановку аргументов и начнёт их вычислять. Так как один из
  аргументов ссылается на new-if (через sqrt-iter), то процедура зациклится.

  Проиллюстрируем это на примере вызова функции sqrt-iter(1, 2):

    (sqrt-iter guess x) => (sqrt-iter 1 2) =>
    => (new-if (good-enough? 1 2) 1 (sqrt-iter (improve 1 2) 2)) => ...
    => (new-if #f 1 (sqrt-iter (improve 1 2) 2)) => ... => (new-if #f 1 (sqrt-iter 1.5 2)) =>
    => (new-if #f 1 (new-if (good-enough? 1.5 2) 1.5 (sqrt-iter (improve 1.5 2) 2)) => ...
    => (new-if #f 1 (new-if #f 1.5 (new-if #f ... (new-if #t 1.995 (new-if #t 1.9975 ... => ...

  Даже когда предикат good-enough? возвращает #t, интерпретатор продолжает подстановку и вычисление
  выражений переданных в качестве аргументов и не попадёт в тело процедуры new-if.

  Легко убедится в этом можно, добавив вывод в консоль в процедурах good-enough?, improve и new-if.
  В консоли мы увидим что-то вроде:

    >"good-enough?"improve"good-enough?"improve"good-enough?"improve"good-enough?"improve"...
|#

#|
(define (new-if predicate then-clause else-clause)
  (newline)
  (write "new-if")
  (newline)
  (cond (predicate then-clause)
        (else else-clause)))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (improve guess x)
  (display "improve")
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (write "good-enough?")
  (< (abs (- (square guess) x))))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                      x)))

(sqrt-iter 1 2)
|#
