#lang sicp

#|
  Упражнение 1.43

  Если f есть численная функция, а n — положительное целое число, то мы можем построить n-кратное
  применение f, которое определяется как функция, значение которой в точке x равно f(f(...(f(x))...)).
  Например, если f есть функция x → x + 1, то n-кратным применением f будет функция x → x + n. Если
  f есть операция возведения в квадрат, то n-кратное применение f есть функция, которая возводит
  свой аргумент в 2ⁿ-ю степень. Напишите процедуру, которая принимает в качестве ввода процедуру,
  вычисляющую f, и положительное целое n, и возвращает процедуру, вычисляющую n-кратное применение f.
  Требуется, чтобы Вашу процедуру можно было использовать в таких контекстах:

    ((repeated square 2) 5)
    625

  Подсказка: может оказаться удобно использовать compose из упражнения 1.42.
|#

(#%require rackunit)

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (repeated-rec n)
    (if (= n 1)
        f
        (compose f (repeated-rec (- n 1)))))
  
  (repeated-rec n))

(define (inc n)
  (+ n 1))

(define (square x)
  (* x x))

(check-equal? ((repeated square 1) 6) 36)
(check-equal? ((repeated square 2) 5) 625)
(check-equal? ((repeated inc 10) 10) 20)