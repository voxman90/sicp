#lang sicp

#|
  Упражнение 1.24

  Модифицируйте процедуру timed-prime-test из упражнения 1.22 так, чтобы она использовала
  fast-prime? (метод Ферма) и проверьте каждое из 12 простых чисел, найденных в этом упражнении.
  Исходя из того, что у теста Ферма порядок роста Θ(log n), то какого соотношения времени Вы бы
  ожидали между проверкой на простоту поблизости от 1 000 000 и поблизости от 1000? Подтверждают
  ли это Ваши данные? Можете ли Вы объяснить наблюдаемое несоответствие, если оно есть?
|#

(define (square n)
  (* n n))

(define (for i pred? inc body)
  (if (pred? i)
      (begin
        (body i)
        (for (inc i) pred? inc body))))

(define (nearest-odd n)
  (if (even? 0)
      (+ n 1)
      n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (dec n)
  (- n 1))

(define (** base degree)
  (define (fast-expt base degree acc)
    (cond ((= degree 0) acc)
          ((even? degree)
           (fast-expt (* base base)
                      (/ degree 2)
                      acc))
          (else
           (fast-expt base
                      (dec degree)
                      (* acc base)))))

  (fast-expt base degree 1))

#|
  Обозначим вероятность достоверного результата теста Ферма как p(prime-fermat?). Необходимое
  число шагов для придания тесту такой достоверности можно рассчитать так:

    steps(prime-fermat?) = n, где (1 - 1/2ⁿ) ≳ p(prime-fermat?)
|#

(define (probability-to-try-amount probability)
  (define try-amount 1)
  (define try-amount-to-prob (/ 1 2))
  ((lambda (self) (self self))
   (lambda (self)
     (cond ((> probability try-amount-to-prob)
            (begin
              (set! try-amount (+ try-amount 1))
              (set! try-amount-to-prob (- 1 (/ 1 (** 2 try-amount))))
              (self self))))))
  try-amount)

(define FERMAT_TEST_PROBABILITY 0.99999)
(define FERMAT_TEST_TRY_AMOUNT (probability-to-try-amount FERMAT_TEST_PROBABILITY))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (get-number-test-time n start-time prime?)
  (if (prime? n FERMAT_TEST_TRY_AMOUNT)
      (- (runtime) start-time)
      0))

(define (get-primes-test-time start prime?)
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
                                      (runtime)
                                      prime?))))

  acc)

(define PRIME_COUNT 100.0)
(define RUN_COUNT 1000)

(define (get-test-time-median start prime?)
  (define acc 0)
  (define start-odd (nearest-odd start))

  (for 0
       (lambda (i) (< i RUN_COUNT))
       (lambda (i) (+ i 1))
       (lambda (_) (set! acc (+ acc (/ (get-primes-test-time start-odd prime?)
                                       PRIME_COUNT)))))

  (/ acc RUN_COUNT))

(define (get-test-time-median-fermat start)
  (get-test-time-median start fast-prime?))


(get-test-time-median-fermat 1000)
(get-test-time-median-fermat 10000)
(get-test-time-median-fermat 100000)
(get-test-time-median-fermat 1000000)
(get-test-time-median-fermat 10000000)
(get-test-time-median-fermat 100000000)
(get-test-time-median-fermat 1000000000)
(newline)

#|
  Проведём замеры для первых ста простых чисел из диапазана (для тысячи повторений) и
  точности теста 99.999%:

    +-------------+--------------------+---------+
    |  Диапазон   | Среднее время (ms) | Прирост |
    +-------------+--------------------+---------+
    | (10³, 10⁴)  |            11.3381 |         |
    | (10⁴, 10⁵)  |            13.7301 |  1.2109 |
    | (10⁵, 10⁶)  |            15.8463 |  1.1541 |
    | (10⁶, 10⁷)  |            18.9705 |  1.1971 |
    | (10⁷, 10⁸)  |            21.6959 |  1.1436 |
    | (10⁸, 10⁹)  |            25.4839 |  1.1745 |
    | (10⁹, 10¹⁰) |            28.1339 |  1.1040 |
    +-------------+--------------------+---------+

  Т.к. log(1 000 000) = log(1000²) = 2*log(1000), то затрачиваемое время на проверку простоты чисел
  близких к n² должно удваиваться по сравнению с числами близкими к n.
  Замеры показывают, что затраты времени растут медленнее, около 1.4-1.6.

  Я ожидал, что затрачиваемое время будет расти быстрее, чем в два раза, т.к. операции с большими
  числами должны занимать чуть больше времени чем операции с малыми. Так же я проверил работу
  процедуры random и она даёт примерно равные результаты для всех диапазонов. Более того, если
  брать в качестве делителей числа близкие к n, это несколько замедлит алгоритм, но ничего
  близкого к 2 получить всё равно не удалось.
|#
