#lang sicp

#|
  Упражнение 1.25

  Лиза П. Хакер жалуется, что при написании expmod мы делаем много лишней работы. В конце концов,
  говорит она, раз мы уже знаем, как вычислять степени, можно просто написать

    (define (expmod base exp m)
      (remainder (fast-expt base exp) m))
  
  Права ли она? Стала бы эта процедура столь же хорошо работать при проверке простых чисел?
  Объясните.
|#

#|
  Лиза П. Хакер не права. Затрачиваемое время расчтёт невероятно быстро в этом случае.

  Потому что при изменении процедуры таким образом приходится иметь дело с огромными числами,
  поиск остатка от деления которых занимает очень много времени.

  В первом случае мы имели дело с числами из следующего диапазона (на каждом шаге):

    1 ≤ a ≤ m - 1, 1 ≤ a² ≤ (m - 1)² = m² - 2m + 1

  Во втором, степень ищется за Θ(log a), но остаток искать приходится от числа, которое
  может приближаться к mᵐ и это очень медленный процесс для больших чисел.

  Убедимся в этом на практике:
|#

(define (square n)
  (* n n))

(define (dec n)
  (- n 1))

(define (for i pred? inc body)
  (if (pred? i)
      (begin
        (body i)
        (for (inc i) pred? inc body))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

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

(define (expmod-slow base exp m)
  (remainder (** base exp) m))

(define (remainder-test a m expmod)
  (= (expmod a m m) m))

(define (get-number-test-time a m start-time expmod)
  (remainder-test a m expmod)
  (- (runtime) start-time))

(define RUN_COUNT 10.0)

(define (get-remainder-test-time-median a m expmod)
  (define acc 0)

  (for 0
       (lambda (i) (< i RUN_COUNT))
       (lambda (i) (+ i 1))
       (lambda (_) (set! acc (+ acc (get-number-test-time a m (runtime) expmod)))))
  (/ acc RUN_COUNT))

(define (print-result f-name a m time)
  (map display
    (list f-name ": a = " a "; m = " m "; avrg. time: " time " ms \n"))
  (newline))

(define (get-test-time-median-slow a m)
  (print-result "expmod-slow" a m (get-remainder-test-time-median a m expmod-slow)))

(define (get-test-time-median a m)
  (print-result "expmod" a m (get-remainder-test-time-median a m expmod)))

(get-test-time-median 500 1000)
(get-test-time-median 999 1000)
(get-test-time-median-slow 500 1000)
(get-test-time-median-slow 999 1000)
(get-test-time-median 5000 1000)
(get-test-time-median 9999 10000)
(get-test-time-median-slow 5000 10000)
(get-test-time-median-slow 9999 10000)
(get-test-time-median 50000 100000)
(get-test-time-median 99999 100000)
(get-test-time-median-slow 50000 100000)
(get-test-time-median-slow 99999 100000)
