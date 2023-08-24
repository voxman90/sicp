#lang sicp

#|
  Упражнение 1.19

  Существует хитрый алгоритм получения чисел Фибоначчи за логарифмическое число шагов.
  Вспомните трансформацию переменных состояния a и b процесса fib-iter из раздела 1.2.2:
  a ← a + b и b ← a. Назовем эту трансформацию T и заметим, что n-кратное применение T,
  начиная с 1 и 0, дает нам пару Fib(n + 1) и Fib(n). Другими словами, числа Фибоначчи
  получаются путем применения Tⁿ, n-ой степени трансформации T, к паре (1, 0). Теперь
  рассмотрим T как частный случай p = 0, q = 1 в семействе трансформаций Tpq, где Tpq
  преобразует пару (a, b) по правилу a ← bq + aq + ap, b ← bp + aq. Покажите, что
  двукратное применение трансформации Tpq равносильно однократному применению трансформации
  Tp′q′ того же типа, и вычислите p′ и q′ через p и q. Это дает нам прямой способ возводить
  такие трансформации в квадрат, и таким образом, мы можем вычислить Tⁿ с помощью
  последовательного возведения в квадрат, как в процедуре fast-expt. Используя все
  эти идеи, завершите следующую процедуру, которая дает результат за логарифмическое
  число шагов:

    (define (fib n)
      (fib-iter 1 0 0 1 n))
    (define (fib-iter a b p q count)
      (cond ((= count 0) b)
            ((even? count)
            (fib-iter a
                      b
                      <??>      ; compute p'
                      <??>      ; compute q'
                      (/ count 2)))
            (else (fib-iter (+ (* b q) (* a q) (* a p))
                            (+ (* b p) (* a q))
                            p
                            q
                            (- count 1)))))
|#

#|
    Tpq: a ← bq + aq + ap,
         b ← bp + aq

    Tpq²: a ← (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p,
          b ← (bp + aq)p + (bq + aq + ap)q

          a ← bpq + aq² + bq² + aq² + apq + bqp + aqp + ap²,
          b ← bp² + aqp + bq² + aq² + apq

          a ← bq² + 2bpq + aq² + 2apq + ap² + aq²,
          b ← bp² + bq² + aq² + 2apq

          a ← b(2pq + q²) + a(2pq + q²) + a(p² + q²),
          b ← b(p² + q²) + a(2pq + q²)

    Tp′q′: a ← bq′ + aq′ + ap′,
           b ← bp′ + aq′

           p′ ← p² + q²,
           q′ ← 2pq + q²
|#

(#%require rackunit)

(define (square a)
  (* a a))

(define (T2p p q)
  (+ (square p)
      (square q)))

(define (T2q p q)
  (+ (* 2 p q)
      (square q)))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
          (fib-iter a
                    b
                    (T2p p q)
                    (T2q p q)
                    (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(check-equal? (fib 0) 0)
(check-equal? (fib 1) 1)
(check-equal? (fib 2) 1)
(check-equal? (fib 5) 5)
(check-equal? (fib 7) 13)
(check-equal? (fib 8) 21)
(check-equal? (fib 10) 55)
