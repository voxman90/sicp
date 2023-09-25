#lang sicp

#|
  Упражнение 3.1

  Накопитель (accumulator) — это процедура, которая вызывается с одним численным аргументом и собирает
  свои аргументы в сумму. При каждом вызове накопитель возвращает сумму, которую успел накопить. Напишите
  процедуру make-accumulator, порождающую накопители, каждый из которых поддерживает свою отдельную
  сумму. Входной параметр make-accumulator должен указывать начальное значение суммы, например:

    (define A (make-accumulator 5))
    (A 10)
    15
    (A 10)
    25
|#

(#%require rackunit)

(define (make-accumulator balance)
  (define (accumulator income)
    (set! balance (+ balance income))
    balance)

  accumulator)

(define acc1 (make-accumulator 10))
(define acc2 (make-accumulator 1000))

(check-equal? (acc1 0) 10)
(check-equal? (acc1 10) 20)
(check-equal? (acc1 10) 30)
(check-equal? (acc2 10) 1010)
(check-equal? (acc2 101) 1111)
