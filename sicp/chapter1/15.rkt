#lang sicp

#|
  Упражнение 1.15

  Синус угла (заданного в радианах) можно вычислить, если воспользоваться приближением sin x ≈ x
  при малых x и употребить тригонометрическое тождество:

    sin(x) = 3sin(x/3) − 4sin³(x/3)

  для уменьшения значения аргумента sin. (В этом упражнении мы будем считать, что угол «достаточно
  мал», если он не больше 0.1 радиана.) Эта идея используется в следующих процедурах:

    (define (cube x) (* x x x))

    (define (p x) (- (* 3 x) (* 4 (cube x))))

    (define (sine angle)
      (if (not (> (abs angle) 0.1))
          angle
          (p (sine (/ angle 3.0)))))

  а. Сколько раз вызывается процедура p при вычислении (sine 12.15)?

  б. Каковы порядки роста в терминах количества шагов и используемой памяти (как функция a) для
  процесса, порождаемого процедурой sine при вычислении (sine a)?
|#

(define (cube x)
  (* x x x))

(define (set-triplet angle sin-x steps)
  (define (triplet f)
    (f angle sin-x steps))
  triplet)

(define (get-angle triplet)
  (define (f angle _ __) angle)
  (triplet f))

(define (get-sin-x triplet)
  (define (f _ sin-x __) sin-x)
  (triplet f))

(define (get-steps triplet)
  (define (f _ __ steps) steps)
  (triplet f))

(define (clone-triplet triplet)
  (set-triplet (get-angle triplet)
               (get-sin-x triplet)
               (get-steps triplet)))

(define (inc-steps triplet)
  (set-triplet (get-angle triplet)
               (get-sin-x triplet)
               (+ (get-steps triplet) 1)))

(define (depl-angle triplet)
  (set-triplet (/ (get-angle triplet) 3.0)
               (get-sin-x triplet)
               (get-steps triplet)))

(define (set-sin-x triplet sin-x)
  (set-triplet (get-angle triplet)
               sin-x
               (get-steps triplet)))

(define (sin-x-to-sin-3x triplet)
  (define sin-x (get-sin-x triplet))
  (set-sin-x triplet (- (* 3 sin-x) (* 4 (cube sin-x)))))

(define (displayln str)
  (display str)
  (newline))

(define (print-triplet triplet)
  (displayln (get-angle triplet))
  (displayln (get-sin-x triplet))
  (displayln (get-steps triplet)))

(define (sine-steps triplet)
  (define angle (get-angle triplet))
  (define triplet-next (inc-steps triplet))
  (if (not (> (abs angle) 0.1))
      (set-sin-x triplet-next angle)
      (clone-triplet (sin-x-to-sin-3x (sine-steps (depl-angle triplet-next))))))

(print-triplet (sine-steps (set-triplet 12.15 0 0)))
(print-triplet (sine-steps (set-triplet 81 0 0)))

#|
    (sine 12.15) =>
    (p (sine 4.05)) =>
    (p (p (sine 1.35))) =>
    (p (p (p (sine 0.45)))) =>
    (p (p (p (p (sine 0.15))))) =>
    (p (p (p (p (p (sine 0.05)))))) =>
    (p (p (p (p (p 0.05))))) =>
    (p (p (p (p sin(0.15))))) =>
    (p (p (p sin(0.45)))) =>
    (p (p sin(1.35))) =>
    (p sin(4.05)) =>
    sin(12.15)

  Число шагов (sine α) обозначим как S(α), где α - угол в радианах.

    S(12.15) = 6

  Найдём формулу для подсчёта числа шагов.
  Число шагов - это наименьшее количество делений на 3, необходимых для того, чтобы угол α 
  стал меньше 0.1.

    |α|/3ⁿ⁻¹ ≤ 0.1; |α| ≤ 3ⁿ⁻¹/10; 10|α|| ≤ 3ⁿ⁻¹; log₃(10|α|) ≤ n - 1; log₃10 + log₃|α| ≤ n - 1;

    log₃30 + log₃|α| ≤ n; 3 = log₃27 < log₃30 < log₃81 = 4;

  Т.к. нас интересует именно наименьшее количество делений и количество шагов есть
  натуральное число, то:

    S(α) = ⌈log₃30 + log₃|α|⌉

  Проверим для 12.15:

    S(12.5) = ⌈log₃30 + log₃|12.15|⌉ = ⌈5.369070⌉ = 6

  Проверим для 81:

    S(81) = ⌈log₃30 + log₃|81|⌉ = ⌈log₃30 + 4⌉ = 8

  Из формулы ясно, что S(α) ⊂ θ(log(α)). Размер стека у нас так же θ(log(α)).

  Почему основание игнорируется? Потому что, log₃α = logᵦ(α)/logᵦ(3), где (logᵦ(3))⁻¹ - константа.
|#
