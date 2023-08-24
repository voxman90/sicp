#lang sicp

#|
  Упражнение 1.20

  Процесс, порождаемый процедурой, разумеется, зависит от того, по каким правилам работает
  интерпретатор. В качестве примера рассмотрим итеративную процедуру gcd, приведенную выше.
  Предположим, что мы вычисляем эту процедуру с помощью нормального порядка, описанного в
  разделе 1.1.5. (Правило нормального порядка вычислений для if описано в упражнении 1.5.)
  Используя подстановочную модель для нормального порядка, проиллюстрируйте процесс, порождаемый
  при вычислении (gcd 206 40) и укажите, какие операции вычисления остатка действительно выполняются.
  Сколько операций remainder выполняется на самом деле при вычислении (gcd 206 40) в нормальном
  порядке? При вычислении в аппликативном порядке?
|#

(#%require rackunit)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(check-equal? (gcd 1 0) 1)
(check-equal? (gcd 0 1) 1)
(check-equal? (gcd 41 17) 1)
(check-equal? (gcd 36 81) 9)

#|
  Для начала, опишем развитие процесса порождаемого процедурой (gcd 206 40) при аппликативном
  порядке вычислений:

    (gcd 206 40) => (if (= 40 0) ...) => ... =>
    (gcd 40 (remainder 206 40)) => (gcd 40 6) => (if (= 6 0) ...) => ... =>
    (gcd 6 (remainder 40 6)) => (gcd 6 4) => (if (= 4 0) ...) => ... =>
    (gcd 4 (remainder 6 4)) => (gcd 4 2) => (if (= 2 0) ...) => ... =>
    (gcd 2 (remainder 4 2)) => (gcd 2 0) => (if (= 0 0) ...) =>
    2

  Остаток вычисляется 4 раза, процедура выполняется пять. Т.е. для аппликативного порядка
  вычислений количество вычислений remainder равно S(a, b) - 1, где S(a, b) количество вызовов
  gcd.
|#

(define (gcd-steps a b steps)
  (if (= b 0)
      (+ steps 1)
      (gcd-steps b (remainder a b) (+ steps 1))))

(check-equal? (gcd-steps 1 0 0) 1)  ; 0 1 => 1
(check-equal? (gcd-steps 0 1 0) 2)  ; 0 1 => 1 0 => 1
(check-equal? (gcd-steps 8 2 0) 2)  ; 8 2 => 2 0 => 2
(check-equal? (gcd-steps 11 3 0) 4) ; 11 3 => 3 2 => 2 1 => 1 0 => 1
(check-equal? (gcd-steps 206 40 0) 5)

(define (gcd-remainder-calls a b)
  (define remainder-calls-count 0)
  (define (remainder-spy a b)
    (set! remainder-calls-count (+ remainder-calls-count 1))
    (remainder a b))
  (define (gcd-remainder a b)
    (if (= b 0)
        remainder-calls-count
        (gcd-remainder b (remainder-spy a b))))
  (gcd-remainder a b))

(check-equal? (gcd-remainder-calls 1 0) 0)
(check-equal? (gcd-remainder-calls 100 33) (gcd-steps 100 33 -1))
(check-equal? (gcd-remainder-calls 36 81) (gcd-steps 36 81 -1))
(check-equal? (gcd-remainder-calls 999 88) (gcd-steps 999 88 -1))

#|
  Теперь опишем развитие процесса порождаемого процедурой (gcd 206 40) при нормальном порядке
  вычислений:

    (gcd 206 40) =>
      (if (= 40 0) ...) => ... =>
    (gcd 40 (reminder 206 40)) =>
      (if (= (reminder 206 40) 0) ...) =>
      (if (= 6 0) ...) => ... =>
    (gcd (reminder 206 40) (reminder 40 (reminder 206 40))) =>
      (if (= (reminder 40 (reminder 206 40)) 0) ...) =>
      (if (= (reminder 40 6) 0) ...) =>
      (if (= 4 0) ...) => ... =>
    (gcd (reminder 40 (reminder 206 40))
         (reminder (reminder 206 40)
                   (reminder 40 (reminder 206 40)))) =>
      (if (= (reminder (reminder 206 40)
                       (reminder 40 (reminder 206 40))) 0) ...) => ... =>
      (if (= 2 0) ...) => ... =>
    (gcd (reminder (reminder 206 40)
                   (reminder 40 (reminder 206 40)))
         (reminder (reminder 40 (reminder 206 40))
                   (reminder (reminder 206 40)
                             (reminder 40 (reminder 206 40)))) =>
      (if (= (reminder (reminder 40 (reminder 206 40))
                       (reminder (reminder 206 40)
                                 (reminder 40 (reminder 206 40)))) 0) ...) => ... =>
      (if (= 0 0) ...) => ... =>
      (reminder (reminder 206 40)
                (reminder 40 (reminder 206 40))) => ... =>
      2

  Количество вызовов gcd то же, что и при аппликативном порядке вычислений.
  На каждом шаге remainder вычисляется только в предикате if (т.к. эта специальная конструкция
  игнорирует "ленивый" характер вычислений и всегда вычисляет предикат прежде чем переходить к
  основной или побочной ветке) и столько раз, сколько её вызов присутствует в b (за исключением
  последнего шага, где она вычисляется ещё и для a).
  Обозначим количество вызовов процедуры remainder в gcd на i-ом шаге как R(aᵢ, bᵢ), где aᵢ и bᵢ -
  аргументы gcd на i-ом шаге. Обозначим количесвто вызовов remainder в произвольном выражении e
  как R(e). Будем считать, что gcd завершает работу за k шагов.
  Тогда:

    R(a₁) = 0, R(b₁) = 0 (если аргументы изначально не включают в себя remainder)

    R(aᵢ) = R(bᵢ₋₁), R(bᵢ) = R(aᵢ₋₁) + R(bᵢ₋₁) + 1

    R(aᵢ, bᵢ) = R(bᵢ), i < k

    R(aₖ, bₖ) = R(aₖ) + R(bₖ) = R(bₖ₋₁) + R(bₖ)

  Получаем подозрительно знакомую последовательность:

    (0, 0) => (0, 1) => (1, 2) => (2, 4) => (4, 7) => (7, 12) => (12, 20) => (20, 33) => ...

    R(a₁) = Fib(1) - 1, R(b₁) = Fib(2) - 1

    R(a₂) = Fib(2) - 1, R(b₂) = (Fib(1) - 1 + Fib(2) - 1) + 1 = Fib(3) - 1

    ...

    R(aᵢ) = Fib(i) - 1, R(bᵢ) = Fib(i + 1) - 1

  Тогда:

    R(gcd) = ∑ᵢR(aᵢ, bᵢ) + R(aₖ, bₖ), где i целое из [1, k - 1]

    R(gcd) = ∑ᵢR(bᵢ)  + R(bₖ) + R(bₖ₋₁) = R(b₁) + ... + R(bₖ₋₁) + R(bₖ) + R(bₖ₋₁) =

    = Fib(2) - 1 + Fib(3) - 1 + ... + Fib(k) - 1 + Fib(k + 1) - 1 + Fib(k) - 1 =

    = Fib(2) + ... + Fib(k + 1) + Fib(k) - (k + 1) =

  Упростим, приняв во внимание, что: Fib(k) = Fib(k + 1) - Fib(k - 1)

    = Fib(3) - Fib(1) + Fib(4) - Fib(2) + Fib(5) - ... + Fib(k + 2) - Fib(k) + Fib(k) - (k + 1) =

    = Fib(k + 1) + Fib(k + 2) - Fib(2) - Fib(1) + Fib(k) - (k + 1) =

    = Fib(k + 3) + Fib(k) - (k + 3) = 2*Fib(k + 2) - (k + 3)

  Проверим полученную формулу на практике:
|#

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

(define (R k)
  (+ (* (fib (+ k 2)) 2)
     (- (+ k 3))))

(check-equal? (R 1) 0)
(check-equal? (R 2) 1)
(check-equal? (R 3) 4)
(check-equal? (R 4) 9)
(check-equal? (R 5) 18)
