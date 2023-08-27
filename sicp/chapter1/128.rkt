#lang sicp

#|
  Упражнение 1.28

  Один из вариантов теста Ферма, который невозможно обмануть, называется тест Миллера–Рабина
  (Miller-Rabin test) (Miller 1976; Rabin 1980). Он основан на альтернативной формулировке Малой
  теоремы Ферма, которая состоит в том, что если n — простое число, а a — произвольное
  положительное целое число, меньшее n, то a в n − 1 ой степени равняется 1 по модулю n. Проверяя
  простоту числа n методом Миллера–Рабина, мы берем случайное число a < n и возводим его в
  (n − 1)-ю степень по модулю n с помощью процедуры expmod. Однако когда в процедуре expmod мы
  проводим возведение в квадрат, мы проверяем, не нашли ли мы «нетривиальный квадратный корень
  из 1 по модулю n», то есть число, не равное 1 или n − 1, квадрат которого по модулю n равен 1.
  Можно доказать, что если такой нетривиальный квадратный корень из 1 существует, то n не простое
  число. Можно, кроме того, доказать, что если n — нечетное число, не являющееся простым, то по
  крайней мере для половины чисел a < n вычисление a^(n − 1) с помощью такой процедуры обнаружит
  нетривиальный квадратный корень из 1 по модулю n (вот почему тест Миллера–Рабина невозможно
  обмануть). Модифицируйте процедуру expmod так, чтобы она сигнализировала обнаружение нетривиального
  квадратного корня из 1, и используйте ее для реализации теста Миллера–Рабина с помощью процедуры
  miller-rabin-test, аналогичной fermat-test. Проверьте свою процедуру на нескольких известных Вам
  простых и составных числах. Подсказка: удобный способ заставить expmod подавать особый сигнал —
  заставить ее возвращать 0.
|#

(#%require rackunit)

(define (square n)
  (* n n))

(define (for i pred? inc body)
  (if (pred? i)
      (begin
        (body i)
        (for (inc i) pred? inc body))))

(define (expmod base exp m)
  (define (remainder-of-square expmod-res)
    (define remainder-res (remainder (square expmod-res) m))
    (if (= remainder-res 1)
        0
        remainder-res))
  (define (expmod-rec exp)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder-of-square (expmod-rec (/ exp 2))))
          (else
           (remainder (* base (expmod-rec (- exp 1))) m))))
  (expmod-rec exp))

(define PRIME_TEST_TIMES 25.0)

(define (prime? n)
  (define (miller-rabin-test n)
    (define (try-it a)
      (expmod a n n))
    (try-it (+ 2 (random (- n 2)))))

  (define (prime-rec? n times)
    (cond ((= times 0) true)
          ((= (miller-rabin-test n) 0) (prime-rec? n (- times 1)))
          (else false)))

  (if (even? n)
      (= n 2)
      (prime-rec? n PRIME_TEST_TIMES)))

(define miller-rabin-test prime?)

(define (print-miller-rabin-test-result n)
  (map display
    (list "(prime? " n ") => " (prime? n)))
  (display "\n"))

(define CARMICHAEL_NUMBERS '(561 1105 1729 2465 2821 6601))
(define PRIME_NUMBERS '(2 3 5 7 11 563 1103 1733 2467 2819 6599))
(define NON_PRIME_NUMBERS '(4 562 1104 1735 2469 2820 6598))

((lambda ()
  (display "Carmichael numbers:\n")
  (map print-miller-rabin-test-result CARMICHAEL_NUMBERS)
  (newline)))

((lambda ()
  (display "Prime numbers:\n")
  (map print-miller-rabin-test-result PRIME_NUMBERS)
  (newline)))

((lambda ()
  (display "Non prime numbers:\n")
  (map print-miller-rabin-test-result NON_PRIME_NUMBERS)
  (newline)))

(check-equal? (miller-rabin-test 3) #t)
(check-equal? (miller-rabin-test 4) #f)
(check-equal? (miller-rabin-test 561) #f)
(check-equal? (miller-rabin-test 1105) #f)
(check-equal? (miller-rabin-test 1729) #f)
(check-equal? (miller-rabin-test 2465) #f)
(check-equal? (miller-rabin-test 2821) #f)
(check-equal? (miller-rabin-test 6601) #f)
(check-equal? (miller-rabin-test 19999) #f)
(check-equal? (miller-rabin-test 1999) #t)
