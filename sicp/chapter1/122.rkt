#lang sicp

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

(define (? pred? then-cond else-cond)
  (if pred?
      then-cond
      else-cond))

(define (nearest-odd n)
  (if (= (remainder n 2) 0)
      (+ n 1)
      n))

(define (report-prime elapsed-time)
  (display " *** ")
  (display (floor elapsed-time)))

(define (start-prime-test n start-time)
  (define is-prime (prime? n))
  (if is-prime
      (report-prime (- (runtime) start-time)))
  is-prime)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (search-primes range-start range-end count)
  (if (not (or (> range-start range-end)
               (= count 0)))
      (search-primes (+ range-start 2)
                     range-end
                     (? (timed-prime-test range-start) (- count 1) count))))

(define PRIME_COUNT 5.0)

(define (search-for-primes range-start range-end)
  (search-primes (nearest-odd range-start) range-end PRIME_COUNT))

(search-for-primes 1000 10000)
(search-for-primes 10000 100000)
(search-for-primes 100000 1000000)
(search-for-primes 1000000 10000000)
(search-for-primes 10000000 100000000)
(newline)

#|
  Для небольших чисел (близких к 1000 или 10000) затрачиваемое время растёт медленнее √10.
  С ростом чисел оценка затрат времени Θ(√n) хорошо описывает процесс и согласуется с
  замерами.

  Проведём серию тестов и получим среднее значение большего количества замеров (так же
  исключим взаимодействие с консолью в ходе проверок):
|#

(define (for i pred? inc body)
  (if (pred? i)
      (begin
        (body i)
        (for (inc i) pred? inc body))))

(define (get-number-test-time n start-time)
  (if (prime? n)
      (- (runtime) start-time)
      0))

(define (get-primes-test-time start)
  (define acc 0)
  (define count 0)

  (for 0
       (lambda (_) (< count PRIME_COUNT))
       (lambda (i) (+ i 1))
       (lambda (i)
               ((lambda (test-time)
                  (if (not (= 0 test-time))
                        (begin
                          (set! count (+ count 1))
                          (set! acc (+ acc test-time)))))
                (get-number-test-time (+ start (* 2 i))
                                      (runtime)))))
  acc)

(define RUN_COUNT 1000)

(define (get-test-time-median start)
  (define acc 0)

  (for 0
       (lambda (i) (< i RUN_COUNT))
       (lambda (i) (+ i 1))
       (lambda (_)
               (set! acc (+ acc
                            (/ (get-primes-test-time (nearest-odd start))
                               PRIME_COUNT)))))
  (/ acc RUN_COUNT))

(get-test-time-median 1000)
(get-test-time-median 10000)
(get-test-time-median 100000)
(get-test-time-median 1000000)
(get-test-time-median 10000000)
(get-test-time-median 100000000)
(get-test-time-median 1000000000)

#|
  Среднее время на проверку на простоту первых пяти простых чисел (за 1000 повторений):

    +--------------+--------------------+------+
    |   Интервал   | Среднее время (мс) | Рост |
    +--------------+--------------------+------+
    |  (10³, 10⁴)  |            1.0084  |      |
    |  (10⁴, 10⁵)  |            2.0048  | 1.99 |
    |  (10⁵, 10⁶)  |            5.9526  | 2.97 |
    |  (10⁶, 10⁷)  |            18.366  | 3.08 |
    |  (10⁷, 10⁸)  |            58.230  | 3.17 |
    |  (10⁸, 10⁹)  |            182.54  | 3.13 |
    | (10⁹, 10¹⁰)  |           580.672  | 3.18 |
    +--------------+--------------------+------+
|#
