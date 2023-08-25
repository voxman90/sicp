#lang sicp

#|
  Упражнение 1.23

  Процедура smallest-divisor в начале этого раздела проводит множество лишних проверок: после
  того, как она проверяет, делится ли число на 2, нет никакого смысла проверять делимость на
  другие четные числа. Таким образом, вместо последовательности 2, 3, 4, 5, 6 ..., используемой
  для test-divisor, было бы лучше использовать 2, 3, 5, 7, 9 ... . Чтобы реализовать такое
  улучшение, напишите процедуру next, которая имеет результатом 3, если получает 2 как аргумент,
  а иначе возвращает свой аргумент плюс 2. В smallest-divisor используйте (next test-divisor)
  вместо (+ test-divisor 1). Используя процедуру timed-prime-test с модифицированной версией
  smallest-divisor, запустите тест для каждого из 12 простых чисел, найденных в упражнении 1.22.
  Поскольку эта модификация снижает количество шагов проверки вдвое, Вы должны ожидать двукратного
  ускорения проверки. Подтверждаются ли эти ожидания? Если нет, то каково наблюдаемое соотношение
  скоростей двух алгоритмов, и как Вы объясните то, что оно отличается от 2?
|#

(define (for i pred? inc body)
  (if (pred? i)
      (begin
        (body i)
        (for (inc i) pred? inc body))))

(define (? pred? then-cond else-cond)
  (if pred?
      then-cond
      else-cond))

(define (nearest-odd n)
  (if (= (remainder n 2) 0)
      (+ n 1)
      n))

(define (square n)
  (* n n))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next n)
  (if (not (= 2 n))
      (+ n 2)
      (+ n 1)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (get-prime-test-time n start-time prime?)
  (if (prime? n)
      (- (runtime) start-time)
      0))

(define (get-first-primes-test-time range-start range-end count prime?)
  (if (not (or (> range-start range-end)
               (= count 0)))
      (+ (get-prime-test-time range-start (runtime) prime?)
         (get-first-primes-test-time (+ range-start 2)
                                     range-end
                                     (? (= (get-prime-test-time range-start (runtime) prime?) 0)
                                        count
                                        (- count 1))
                                     prime?))
      0))

(define PRIME_COUNT 5.0)
(define RUN_COUNT 1000)

(define (get-test-time-median range-start range-end prime?)
  (define acc 0)
  (define range-start-odd (nearest-odd range-start))
  (for 0
       (lambda (i) (< i RUN_COUNT))
       (lambda (i) (+ i 1))
       (lambda (_)
               (set! acc (+ acc
                         (/ (get-first-primes-test-time range-start-odd range-end PRIME_COUNT prime?)
                         PRIME_COUNT)))))
  (/ acc RUN_COUNT))

(define (get-test-time-median-next range-start range-end)
  (get-test-time-median range-start range-end prime?))

(get-test-time-median-next 1000 10000)
(get-test-time-median-next 10000 100000)
(get-test-time-median-next 100000 1000000)
(get-test-time-median-next 1000000 10000000)
(get-test-time-median-next 10000000 100000000)
(get-test-time-median-next 100000000 1000000000)
(get-test-time-median-next 1000000000 10000000000)

#|
  Среднее время на проверку на простоту первых пяти простых чисел (за 1000 повторений):

    +-------------+--------------------+-------+---------------------------+-------------+-------+
    | Интервал    | Среднее время (мс) | Рост  | Среднее время с next (мс) | Рост (next) |       |
    +-------------+--------------------+-------+---------------------------+-------------+-------+
    | (10³, 10⁴)  | 1.0084             |       | 0.8996                    |             | 0.892 |
    | (10⁴, 10⁵)  | 2.0048             | 1.988 | 1.178                     | 1.30        | 0.587 |
    | (10⁵, 10⁶)  | 5.9526             | 2.97  | 3.482                     | 2.956       | 0.585 |
    | (10⁶, 10⁷)  | 18.366             | 3.08  | 9.994                     | 2.87        | 0.544 |
    | (10⁷, 10⁸)  | 58.23              | 3.17  | 30.93                     | 3.1         | 0.531 |
    | (10⁸, 10⁹)  | 182.54             | 3.134 | 96.96                     | 3.135       | 0.531 |
    | (10⁹, 10¹⁰) | 580.672            | 3.18  | 305.69                    | 3.153       | 0.526 |
    +-------------+--------------------+-------+---------------------------+-------------+-------+

  Получаемое ускорение весьма ощутимо, но по подсчётам приближается к 1.9, а не к 2. Одна из
  возможных причин - это то, что количество операций уменьшается не точно вдвое. Ведь мы теперь
  не просто прибавляем к n единицу, но и проверяем, равно ли n двум.

  Уменьшилось бы среднее время проверки на простоту вдвое, если бы нам не пришлось сравнивать
  потенциальный делитель с 2 на каждом шаге?

  Проверим это:
|#

(define (find-divisor-alt n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-alt n (+ test-divisor 2)))))

(define (prime-alt? n)
  (if (divides? 2 n)
    (if (= 2 n) #t #f)
    (= n (find-divisor-alt n 3))))

(define (get-test-time-median-alt range-start range-end)
  (get-test-time-median range-start range-end prime-alt?))

(newline)
(get-test-time-median-alt 1000 10000)
(get-test-time-median-alt 10000 100000)
(get-test-time-median-alt 100000 1000000)
(get-test-time-median-alt 1000000 10000000)
(get-test-time-median-alt 10000000 100000000)
(get-test-time-median-alt 100000000 1000000000)
(get-test-time-median-alt 1000000000 10000000000)

#|
    +-------------+--------------------+------------------------+---------+
    | Интервал    | Среднее время (мс) | Среднее время alt (мс) | Разница |
    +-------------+--------------------+------------------------+---------+
    | (10³, 10⁴)  | 1.0084             | 0.974                  | 0.965   |
    | (10⁴, 10⁵)  | 2.0048             | 1.189                  | 0.593   |
    | (10⁵, 10⁶)  | 5.9526             | 3.13                   | 0.526   |
    | (10⁶, 10⁷)  | 18.366             | 9.376                  | 0.51    |
    | (10⁷, 10⁸)  | 58.23              | 28.907                 | 0.496   |
    | (10⁸, 10⁹)  | 182.54             | 90.57                  | 0.496   |
    | (10⁹, 10¹⁰) | 580.672            | 287.41                 | 0.495   |
    +-------------+--------------------+------------------------+---------+

  Результат существенно ближе к двухкратному уменьшению затрачиваемого времени, но, всё же,
  ускоряется даже несколько быстрее. В альтернативной версии не только убрано сравнение с
  двойкой на каждом шаге, но и меньшее количество вызовов процедур (т.к., как минимум одна
  промежуточная стадия вычислений убрана).
|#
