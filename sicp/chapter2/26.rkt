#lang sicp

#|
  Упражнение 2.6

  Если представление пар как процедур было для Вас еще недостаточно сумасшедшим, то заметьте, что в
  языке, который способен манипулировать процедурами, мы можем обойтись и без чисел (по крайней мере,
  пока речь идет о неотрицательных числах), определив 0 и операцию прибавления 1 так:

    (define zero (lambda (f) (lambda (x) x)))

    (define (add-1 n)
      (lambda (f) (lambda (x) (f ((n f) x)))))

  Такое представление известно как числа Чёрча (Church numerals), по имени его изобретателя, Алонсо
  Чёрча, того самого логика, который придумал λ-исчисление.

  Определите one (единицу) и two (двойку) напрямую (не через zero и add-1). (Подсказка: вычислите
  (add-1 zero) с помощью подстановки.) Дайте прямое определение процедуры сложения + (не в
  терминах повторяющегося применения add-1).
|#

#|
    (define (add-1 (zero)))
    (define (add-1 (lambda (f) (lambda (x) x))))
    (lambda (f) (lambda (x) (f ((lambda (f) (lambda (x) x)) f) x))))
    (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
    (lambda (f) (lambda (x) (f x)))

    (define one (lambda (f) (lambda (x) (f x))))
    (define two (lambda (f) (lambda (x) (f (f x)))))

  И так далее.
|#

(#%require rackunit)

(define zero (lambda (_) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add g1 g2)
  (lambda (f) (lambda (x) ((g2 f) ((g1 f) x)))))

(define (inc x) (+ x 1))

(define (church->nat n) ((n inc) 0))

(check-equal? (church->nat zero) 0)
(check-equal? (church->nat one) 1)
(check-equal? (church->nat two) 2)
(check-equal? (church->nat (add one one)) 2)
(check-equal? (church->nat (add two one)) 3)
(check-equal? (church->nat (add (add two one) zero)) 3)
(check-equal? (church->nat (add (add two one) one)) 4)
