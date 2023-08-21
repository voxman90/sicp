#lang sicp

#|
  Упражнение 1.12

  Приведенная ниже таблица называется треугольником Паскаля (Pascal’s triangle).

        1
       1 1
      1 2 1
     1 3 3 1
    1 4 6 4 1
       ...

  Все числа по краям треугольника равны 1, а каждое число внутри треугольника равно сумме двух
  чисел над ним. Напишите процедуру, вычисляющую элементы треугольника Паскаля с помощью
  рекурсивного процесса.
|#

(#%require rackunit)

(define (dec a)
  (- a 1))

(define (get-Pascal-triangle-coeff row position)
  (if (or (= row position) (= position 1))
      1
      (+ (get-Pascal-triangle-coeff (dec row) (dec position))
         (get-Pascal-triangle-coeff (dec row) position))))

(define solution get-Pascal-triangle-coeff)

(check-equal? (solution 1 1) 1)
(check-equal? (solution 3 2) 2)
(check-equal? (solution 4 3) 3)
(check-equal? (solution 5 2) 4)
(check-equal? (solution 5 3) 6)
(check-equal? (solution 6 3) 10)
