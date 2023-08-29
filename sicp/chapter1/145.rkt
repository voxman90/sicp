#lang sicp

#|
  Упражнение 1.45

  В разделе 1.3.3 мы видели, что попытка вычисления квадратных корней путем наивного поиска
  неподвижной точки y → x/y не сходится, и что это можно исправить путем торможения усреднением.
  Тот же самый метод работает для нахождения кубического корня как неподвижной точки y → x/y²,
  заторможенной усреднением. К сожалению, этот процесс не работает для корней четвертой степени —
  однажды примененного торможения усреднением недостаточно, чтобы заставить сходиться процесс поиска
  неподвижной точки y → x/y³. С другой стороны, если мы применим торможение усреднением дважды (т.е.
  применим торможение усреднением к результату торможения усреднением от y → x/y³), то поиск
  неподвижной точки начнет сходиться. Проделайте эксперименты, чтобы понять, сколько торможений
  усреднением нужно, чтобы вычислить корень n-ой степени как неподвижную точку на основе
  многократного торможения усреднением функции y → x/yⁿ⁻¹. Используя свои результаты для того,
  напишите простую процедуру вычисления корней n-ой степени с помощью процедур fixed-point,
  average-damp и repeated из упражнения 1.43. Считайте, что все арифметические операции, какие
  Вам понадобятся, присутствуют в языке как примитивы.
|#

(#%require rackunit)

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (iter base acc n)
    (cond ((= n 0) acc)
          ((even? n) (iter (compose base base) acc (/ n 2)))
          (else (iter base (compose base acc) (dec n)))))

  (iter f identity n))

(define (n-degree n)
  (define (iter base acc n)
    (cond ((= n 0) acc)
          ((even? n) (iter (compose square base) acc (/ n 2)))
          (else (iter base (lambda (x) (* (acc x) (base x))) (dec n)))))

  (iter identity (lambda (_) 1) n))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define counter 0)

  (define (print-guess guess next-guess)
    (set! counter (+ counter 1))
    (map display (list "№" counter " guess: " guess "; next-guess: " next-guess ";\n")))

  (define (close-enough? guess next-guess)
    (< (abs (- next-guess guess)) tolerance))

  (define (try guess)
    (let ((next-guess (f guess)))
      (print-guess guess next-guess)
      (if (close-enough? guess next-guess)
          next-guess
          (try next-guess))))

  (try first-guess))

(define (nth-root n)
  (define average-dump-func (repeated average-damp
                                      (floor (log n 2))))

  (lambda (y)
    (let ((f (lambda (x) (/ y
                            ((n-degree (dec n)) x)))))
      (fixed-point (average-dump-func f) 1.0))))

(define (square x)
  (* x x))

(check-equal? ((repeated square 1) 6) 36)
(check-equal? ((repeated square 2) 5) 625)
(check-equal? ((repeated inc 10) 10) 20)
(check-equal? ((repeated inc 15) 10) 25)

(check-equal? ((n-degree 1) 2) 2)
(check-equal? ((n-degree 2) 2) 4)
(check-equal? ((n-degree 3) 2) 8)
(check-equal? ((n-degree 4) 2) 16)
(check-equal? ((n-degree 5) 2) 32)
(check-equal? ((n-degree 4) 3) 81)

(define (root3 x) ((nth-root 3) x))
(define (root4 x) ((nth-root 4) x))
(define (root10 x) ((nth-root 10) x))
(define (root30 x) ((nth-root 30) x))

(check-equal? (round (root3 27)) 3.0)
(check-equal? (round (root3 64)) 4.0)
(check-equal? (round (root4 81)) 3.0)
(check-equal? (round (root4 10000)) 10.0)
(check-equal? (round (root10 1024)) 2.0)
(check-equal? (round (root30 1073741824)) 2.0)

#|
  Протестируем процедуру на корнях из степеней семёрки:

    +-----+----+----------------+-------+
    | nth | x  | average dumped | steps |
    +-----+----+----------------+-------+
    |   2 |  7 |              0 | +inf  |
    |   2 |  7 |              1 | 7     |
    |   2 |  7 |              2 | 22    |
    |   2 |  7 |              3 | 2     |
    |   2 |  7 |              4 | 79    |
    +-----+----+----------------+-------+

  Если не тормозить усреднением, то расходится. Если тормозить усреднением слишком много раз, то
  растёт число шагов и возникают проблемы с тем, что прирост становится меньше погрешности.

    +-----+----+----------------+---------+
    | nth | x  | average dumped |  steps  |
    +-----+----+----------------+---------+
    |   3 |  7 |              1 | 20      |
    |   3 |  7 |              2 | 20      |
    |   3 |  7 |              2 | 40      |
    |   4 |  7 |              1 | +inf    |
    |   4 |  7 |              2 | 21      |
    |   5 |  7 |              2 | 31      |
    |   6 |  7 |              2 | 47      |
    |   7 |  7 |              2 | 70      |
    |   8 |  7 |              2 | +inf    |
    |   8 |  7 |              3 | 92      |
    |  16 |  7 |              3 | +inf    |
    |  16 |  7 |              4 | 415     |
    |  15 |  7 |              3 | 265     |
    |  31 |  7 |              4 | 1001    |
    |  32 |  7 |              4 | +inf    |
    |  32 |  7 |              5 | 1796    |
    |  64 |  7 |              6 | 7526    |
    | 128 |  7 |              7 | 30896   |
    | 128 | 11 |              7 | 38214   |
    | 128 | 99 |              7 | 73793   |
    +-----+----+----------------+---------+

  Это работает. avr([127, 64]) = 6, avr([63, 32]) = 5, ..., avr([3, 2]) = 1, avr(1) = 0.
  Получаем, что avr([2ⁿ⁺¹ - 1, 2ⁿ]) = n => avr(m) = ⌊log₂m⌋.
|#
