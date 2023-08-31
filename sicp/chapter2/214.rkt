#lang racket

#|
  Упражнение 2.14

  Покажите, что Дайко прав. Исследуйте поведение системы на различных арифметических выражениях.
  Создайте несколько интервалов A и B и вычислите с их помощью выражения A/A и A/B. Наибольшую
  пользу Вы получите, если будете использовать интервалы, радиус которых составляет малую часть
  от среднего значения. Исследуйте результаты вычислений в форме центр/проценты (см. упражнение
  2.12).
|#

(#%require rackunit)

(check-equal? (+ 0.3 0.4) 0.7)
(check-not-equal? (- 0.7 0.4) 0.3)

(check-not-equal? (+ 0.1 0.2) 0.3)
(check-not-equal? (- 0.3 0.2) 0.1)

(check-equal? (/ (+ (* 10 0.1) (* 10 0.2)) 10) 0.3)
(check-equal? (/ (- (* 10 0.7) (* 10 0.4)) 10) 0.3)

#|
  Эти хрестоматийные примеры отлично показывают, что система ведёт себя по разному на разных, но
  алгебраически эквивалентных выражениях.

  Следующие выражения алгебраически эквивалентны:

    (0.7 - 0.4) = 0.3 и (10 * 0.7 - 10 * 0.4)/10 = 0.3,

    (0.1 + 0.2) = 0.3 и (10 * 0.1 + 10 * 0.2)/10 = 0.3

  но они дают разный результат, в первом случае false, во втором true.

  Одна из причин заключается в том, что числа подобные 0.3, представимые с конечным хвостом в системе
  счисления по основанию 10, имеют бесконечный хвост в системе счисления по основанию 2.

  Переведём 0.3 в двоичную систему счисления:

    0.3 * 2, 0.6 * 2, 1.2 * 2, 0.4 * 2, 0.8 * 2, 1.6 * 2, 1.2 * 2, 0.4 * 2, 0.8 * 2, ...

    0.3₁₀ = 0.01(0011)₂

  Система даёт сбой уже на таком базовом уровне, поэтому не удивительно, что результаты могут
  оказаться неравными для более сложных алгебраически эквивалентных выражений.

  Проблема касается не только дробей, но и больших чисел с плавающей запятой:
|#

(define big-number (expt 10.0 16))

(filter (lambda (str) (begin (display str) #f))
  (list "big-number: " big-number
        "\nbig-number + 1: " (+ big-number 1)
        "\nbig-number + 2: " (+ big-number 2)
        "\n\n"))

(check-equal? big-number (+ big-number 1))
(check-not-equal? big-number (+ big-number 2))

#|
  Это конечное число, которое оказывается равно себе плюс один. Парадоксальный и, безусловно, неверный
  результат.

  Т.е. на базовом уровне, многие операции с float дают неверный результат. Это касается не только
  lisp, но и других языков программирования, использующих стандарт IEEE 754 (например, JS).

  Но, возможно, в нашей реализации арифметики интервалов кроется неточность и другого толка.
|#

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (center interval)
  (/ (+ (lower-bound interval)
        (upper-bound interval))
     2))

(define (percent interval)
  (* 100
     (/ (/ (- (upper-bound interval)
              (lower-bound interval))
        2)
        (abs (center interval)))))

(define (zero-boundary? interval)
  (= (* (lower-bound interval)
        (upper-bound interval))
     0))

(define (mul-interval a b)
  (let ((p1 (* (lower-bound a) (lower-bound b)))
        (p2 (* (lower-bound a) (upper-bound b)))
        (p3 (* (upper-bound a) (lower-bound b)))
        (p4 (* (upper-bound a) (upper-bound b))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval a b)
  (if (zero-boundary? b)
      (error "division by zero")
      (mul-interval a
                    (make-interval (/ 1.0 (upper-bound b))
                                   (/ 1.0 (lower-bound b))))))

#|
  Убедимся, что (1, 1) является нейтральным элементом относительно умножения интевалов:

    (1, 1)*(l, u) = (l, u), (l, u)*(1, 1) = (l, u)

  Посмотрим, чему равняется A/A в арифметике интервалов:

    A = (l, u), A/A = (l, u)/(l, u) = (l, u)(1/u, 1/l) =

    = (min(l/l, l/u, u/l, u/u), max(l/l, l/u, u/l, u/u)) = (min(1, l/u, u/l), max(1, l/u, u/l))

  В некоторых случаях, например, если 0 < l < u, A/A не равно (1, 1). Элемент A⁻¹ может не являтся
  обратным для A. Это значит, эквивалентность, понимаемая в обычном для алгебраических преобразований
  смысле, не применима без оговорок для формул, где присутствует деление интервалов.
|#

(define a (make-interval 50.0 150.0))
(define b (make-interval 9.9 10.1))
(define c (make-interval -0.5 1.5))

(check-equal? (mul-interval a '(1 . 1)) a)
(check-equal? (div-interval a '(1 . 1)) a)
(check-not-equal? (div-interval a a) '(1 . 1))

(check-equal? (mul-interval b '(1 . 1)) b)
(check-equal? (div-interval b '(1 . 1)) b)
(check-not-equal? (div-interval b b) '(1 . 1))

(check-equal? (mul-interval c '(1 . 1)) c)
(check-equal? (div-interval c '(1 . 1)) c)
(check-not-equal? (div-interval c c) '(1 . 1))

(check-equal? (percent a) 50.0)
(check-not-equal? (percent b) 10.0) ; !
(check-equal? (percent c) 200.0)

#|
  Собственно, мы сталкиваемся и с последствиями погрешности в представлении чисел с плавающей запятой,
  и с особеностями представления интервалов в нашей программе.
|#
