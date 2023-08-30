#lang sicp

#|
  Упражнение 2.9

  Радиус (width) интервала определяется как половина расстояния между его верхней и нижней границами.
  Радиус является мерой неопределенности числа, которое обозначает интервал. Есть такие математические
  операции, для которых радиус результата зависит только от радиусов интервалов аргументов, а есть
  такие, для которых радиус результата не является функцией радиусов аргументов. Покажите, что радиус
  суммы (или разности) двух интервалов зависит только от радиусов интервалов, которые складываются
  (или вычитаются). Приведите примеры, которые показывают, что для умножения или деления это не так.
|#

#|
  Сумма или разность интервалов (a1, a2), (b1, b2) высчитываются по правилу:

    (a1, a2) + (b1, b2) = (a1 + b1, a2 + b2), (a1, a2) - (b1, b2) = (a1 - b2, a2 - b1)

    w(a, b) = (b - a)/2,

    w(a1, a2) + w(b1, b2) = (a2 - a1)/2 + (b2 - b1)/2 = (a2 - a1 + b2 - b1)/2 =

    = ((a2 + b2) - (a1 + b1))/2 = w(a1 + b1, a2 + b2)

    w(a1 - b2, a2 - b1) = ((a2 - b1) - (a1 - b2))/2 = (a2 - b1 - a1 + b2)/2 =

    = (a2 - a1)/2 + (b2 - b1)/2 = w(a1, a2) + w(b1, b2)

  Получается, что радиус разности и суммы интервалов равен сумме радиусов интервалов.

  Приведём пример, который показывает, что произведение радиусов интервалов не обязательно равно
  радиусу произведения интервалов:

    i = (-4, -2), j = (2, 4), i * j = (-16, -4)

    w(i) = 1, w(j) = 1, w(i * j) = 6

    i / j = (-4, -2) * (1/4, 1/2) = (-2, -1/2)

    w(i) = 1, w(j) = 1, w(i / j) = (-1/2 + 2)/2 = 3/4
|#

(#%require rackunit)

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval))
     2))

(define (add-interval a b)
  (make-interval (+ (lower-bound a) (lower-bound b))
                 (+ (upper-bound a) (upper-bound b))))

(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
                 (- (upper-bound a) (lower-bound b))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define l -4)
(define u -2)

(define l2 2)
(define u2 4)

(define interval (make-interval l u))
(define interval2 (make-interval l2 u2))

(define interval-add (add-interval interval interval2))
(define interval-sub (sub-interval interval interval2))
(define interval-mul (mul-interval interval interval2))
(define interval-div (div-interval interval interval2))

(check-equal? (width interval) (/ (abs(- l u)) 2))

(check-equal? (width interval-add) (+ (width interval)
                                      (width interval2)))

(check-equal? (width interval-sub) (+ (width interval)
                                      (width interval2)))

(check-not-equal? (width interval-mul) (* (width interval)
                                          (width interval2)))

(check-not-equal? (width interval-div) (/ (width interval)
                                          (width interval2)))
