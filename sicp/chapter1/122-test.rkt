#lang racket

#|
  Упражнение 1.22

  Большая часть реализаций Лиспа содержат элементарную процедуру runtime, которая возвращает
  целое число, показывающее, как долго работала система (например, в миллисекундах). Следующая
  процедура timed-prime-test, будучи вызвана с целым числом n, печатает n и проверяет, простое
  ли оно. Если n простое, процедура печатает три звездочки и количество времени, затраченное
  на проверку.

    (define (timed-prime-test n)
      (newline)
      (display n)
      (start-prime-test n (runtime)))
    (define (start-prime-test n start-time)
      (if (prime? n)
          (report-prime (- (runtime) start-time))))
    (define (report-prime elapsed-time)
      (display " *** ")
      (display elapsed-time))

  Используя эту процедуру, напишите процедуру search-for-primes, которая проверяет на простоту
  все нечетные числа в заданном диапазоне. С помощью этой процедуры найдите наименьшие три
  простых числа после 1000; после 10 000; после 100 000; после 1 000 000. Посмотрите, сколько
  времени затрачивается на каждое простое число. Поскольку алгоритм проверки имеет порядок
  роста Θ(√n), Вам следовало бы ожидать, что проверка на простоту чисел, близких к 10 000,
  занимает в √10 раз больше времени, чем для чисел, близких к 1000. Подтверждают ли это Ваши
  замеры времени? Хорошо ли поддерживают предсказание √n данные для 100 000 и 1 000 000?
  Совместим ли Ваш результат с предположением, что программы на Вашей машине затрачивают на
  выполнение задач время, пропорциональное числу шагов?
|#

(#%require rackunit
           racket/string)

(define (square n)
  (* n n))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (runtime) (current-milliseconds))

(define (report-prime elapsed-time out)
  (display " *** " out)
  (displayln (floor elapsed-time) out))

(define (start-prime-test n start-time out)
  (define is-prime (prime? n))
  (when is-prime
    (report-prime (- (runtime) start-time) out))
  is-prime)

(define (timed-prime-test n out)
  (newline)
  (display n out)
  (start-prime-test n (runtime) out))

(define (? pred? then-cond else-cond)
  (if pred?
      then-cond
      else-cond))

(define (search-first-n-primes range-start range-end count out)
  (when (not (or (> range-start range-end) (= count 0)))
    (search-first-n-primes (+ range-start 2)
                           range-end
                           (? (timed-prime-test range-start out) (- count 1) count)
                           out)))

(define (nearest-odd n)
  (if (= (remainder n 2) 0)
      (+ n 1)
      n))

(define (search-for-primes range-start range-end out)
  (search-first-n-primes (nearest-odd range-start) range-end 3 out))

(define op1 (open-output-string))

(search-for-primes 2 7 op1)

(define display-result (get-output-string op1))

(check-equal? (string-replace display-result (regexp " \\*\\*\\* [0-9]+\n") "") "357")

(close-output-port op1)
