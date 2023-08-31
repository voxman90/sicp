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

(check-equal? (+ (* 10 0.1) (* 10 0.2)) (* 10 0.3))
(check-equal? (- (* 10 0.7) (* 10 0.4)) (* 10 0.3))

#|
  Эти хрестоматийные примеры отлично показывают, почему система ведёт себя по разному на разных, но
  алгебраически эквивалентных выражениях.

  Проблема заключается в том, что числа подобные 0.3, представимые с конечным хвостом в системе
  счисления по основанию 10, имеют бесконечный хвост в системе счисления по основанию 2.

  Переведём 0.3 в двоичную систему счисления:

    0.3 * 2, 0.6 * 2, 1.2 * 2, 0.4 * 2, 0.8 * 2, 1.6 * 2, 1.2 * 2, 0.4 * 2, 0.8 * 2, ...

    0.3₁₀ = 0.01(0011)₂

  Трудности возникают и с большими числами. В JS, например, для переменной n типа number, которая
  больше MAX_SAFE_INTEGER, может быть справедливо n === n + 1.

  Проверим это для Lisp:
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
  Судя по результатам, Scheme использует тот же стандарт для представления float, что и JS. Возникают
  те же "странности" в результатах.
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

(define (width interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
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

(define a (make-interval 50.0 150.0))
(define b (make-interval 9.9 11.1))
(define c (make-interval 0.99 1.01))

(filter (lambda (str) (begin (display str) #f))
  (list "a: " a " ac: " (center a) " aw: " (width a) " ap: " (percent a)
        "\na/a: " (div-interval a a) "\na/b: " (div-interval a b) "\na/c: " (div-interval a c)
        "\n\nb: " b " bc: " (center b) " bw: " (width b) " bp: " (percent b)
        "\nb/b: " (div-interval b b) "\nb/a: " (div-interval b a) "\nb/c: " (div-interval b c)
        "\n\nc: " c " cc: " (center c) " cw: " (width c) " cp: " (percent c)
        "\nc/c: " (div-interval c c) "\nc/a: " (div-interval c a) "\nc/b: " (div-interval c b)
        "\n"))

(check-not-equal? (div-interval a a) '(1 . 1))
(check-not-equal? (div-interval b b) '(1 . 1))
(check-not-equal? (div-interval c c) '(1 . 1))

#|
  Результаты показывают, что в ходе вычислений образуется множество периодических дробей. Представлены
  они с погрешностью, что неизбежно, если брать формат float (и чего можно избежать, если представлять
  их в виде рациональных чисел - где числитель целое, а знаменатель - натуральное число).
|#
