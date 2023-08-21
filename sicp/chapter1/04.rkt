#lang sicp

#|
  Упражнение 1.4

  Заметим, что наша модель вычислений разрешает существование комбинаций, операторы которых —
  составные выражения. С помощью этого наблюдения опишите, как работает следующая процедура:

  (define (a-plus-abs-b a b)
    ((if (> b 0) + -) a b))
|#

(#%require rackunit)

#|
  Если b больше нуля, то возвращаем a + b,
  елси b меньше или равно нулю, то возвращаем a - b.
  a-plus-abs-b(a, b) => a + |b|
|#

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(define solution a-plus-abs-b)

(check-equal? (solution 10 20) 30)
(check-equal? (solution 10 (- 20)) 30)
(check-equal? (solution 10 0) 10)
