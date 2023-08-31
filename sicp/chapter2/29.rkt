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
  Сумма или разность интервалов (a₁, a₂), (b₁, b₂) высчитываются по правилу:

    (a₁, a₂) + (b₁, b₂) = (a₁ + b₁, a₂ + b₂), (a₁, a₂) - (b₁, b₂) = (a₁ - b₂, a₂ - b₁)

    w(a, b) = (b - a)/2,

    w(a₁, a₂) + w(b₁, b₂) = (a₂ - a₁)/2 + (b₂ - b₁)/2 = (a₂ - a₁ + b₂ - b₁)/2 =

    = ((a₂ + b₂) - (a₁ + b₁))/2 = w(a₁ + b₁, a₂ + b₂)

    w(a₁ - b₂, a₂ - b₁) = ((a₂ - b₁) - (a₁ - b₂))/2 = (a₂ - b₁ - a₁ + b₂)/2 =

    = (a₂ - a₁)/2 + (b₂ - b₁)/2 = w(a₁, a₂) + w(b₁, b₂)

  Получается, что радиус разности и суммы интервалов равен сумме радиусов интервалов.

  Приведём пример, который показывает, что произведение (отношение) радиусов интервалов может быть 
  не равно радиусу произведения (отношения) интервалов:

    i = (-2, 6), j = (2, 4), i * j = (-8, 24)

    w(i) = 4, w(j) = 1, w(i * j) = 16, w(i)w(j) != w(i * j)

    i/j = (-2, 6) * (1/4, 1/2) = (-1/2, 3)

    w(i) = 4, w(j) = 1, w(i/j) = (3 + 1/2)/2 = 7/4, w(i)/w(j) != w(i/j)

  Покажем для группы частных случаев, что радиус произведения интервалов зависит не только от
  радиуса интервалов:

    i₁ = (c₁ - w₁, c₁ + w₁), i₂ = (c₂ - w₂, c₂ + w₂), где cᵢ - центр, а wᵢ - радиус.

    w(i₁)w(i₂) = ((c₁ + w₁) - (c₁ - w₁))/2 * ((c₂ + w₂) - (c₂ - w₂))/2 = w₁w₂

  Возьмём такие положительные c₁, c₂, w₁, w₂, что c₁ - w₁ < 0 и c₂ - w₂ > 0, тогда:

    w(i₁i₂) = w((c₁ - w₁)(c₂ + w₂), (c₁ + w₁)(c₂ + w₂)) =

    = w(c₁c₂ - w₁c₂ + c₁w₂ - w₁w₂, c₁c₂ + w₁c₂ + c₁w₂ + w₁w₂) =

    = ((c₁c₂ + w₁c₂ + c₁w₂ + w₁w₂) - (c₁c₂ - w₁c₂ + c₁w₂ - w₁w₂))/2 =

    = (2w₁c₂ + 2w₁w₂)/2 = w₁c₂ + w₁w₂ = c₂w(i₁) + w(i₁)w(i₂) ∎
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

(define ac 2)
(define aw 4)
(define al (- ac aw))
(define au (+ ac aw))

(define bc 3)
(define bw 1)
(define bl (- bc bw))
(define bu (+ bc bw))

(define a (make-interval al au))
(define b (make-interval bl bu))

(define width-a (width a))
(define width-b (width b))

(define interval-add (add-interval a b))
(define interval-sub (sub-interval a b))
(define interval-mul (mul-interval a b))
(define interval-div (div-interval a b))

(check-equal? width-a (/ (abs(- al au)) 2))
(check-equal? width-b (/ (abs(- bl bu)) 2))

(check-equal? (width interval-add) (+ width-a width-b))
(check-equal? (width interval-sub) (+ width-a width-b))

; w(i₁i₂) = c₂w(i₁) + w(i₁)w(i₂)
(check-equal? (width interval-mul) (+ (* bc width-a)
                                      (* width-a width-b)))

(check-not-equal? (width interval-mul) (* width-a width-b))
(check-not-equal? (width interval-div) (/ width-a width-b))
