#lang sicp

#|
  Упражнение 2.13

  Покажите, что, если предположить, что погрешность составляет малую долю величины интервала, то
  погрешность в процентах произведения двух интервалов можно получить из погрешности в процентах
  исходных интервалов по простой приближенной формуле. Задачу можно упростить, если предположить,
  что все числа положительные.
|#

#|
  В условии сказано, что погрешность составляет малую долю величины интервала, но не указано, что это
  значит. Меньше процента от величины центра интервала? Настолько малую долю, что ей можно пренебречь?
  Будем считать, что малая доля величины интервала - это величина меньшая процента от центра интервала
  и что этой величиной можно пренебречь.

  Пусть i₁ = (l₁, u₁), i₂ = (l₂, u₂), где l₁, u₁, l₂, u₂ положительные числа. lᵢ = cᵢ - cᵢpᵢ,
  uᵢ = cᵢ + cᵢpᵢ, где cᵢ - центр; pᵢ, p(i) - погрешность в процентах; cᵢpᵢ, w(i) - радиус или
  погрешность.

    i₁ = (c₁ - c₁p₁, c₁ + c₁p₁), i₂ = (c₂ - c₂p₂, c₂ + c₂p₂)

    w(i₁i₂) = w((c₁ - c₁p₁)(c₂ - c₂p₂), (c₁ + c₁p₁)(c₂ + c₂p₂)) =

    = w(c₁c₂ - c₁c₂p₁ - c₁c₂p₂ + c₁c₂p₁p₂, c₁c₂ + c₁c₂p₁ + c₁c₂p₂ + c₁c₂p₁p₂) =

    = (c₁c₂ + c₁c₂p₁ + c₁c₂p₂ + c₁c₂p₁p₂ - c₁c₂ + c₁c₂p₁ + c₁c₂p₂ - c₁c₂p₁p₂)/2 =

    = (2c₁c₂p₁ + 2c₁c₂p₂)/2 = c₁c₂p₁ + c₁c₂p₂ = c₁c₂(p₁ + p₂)

  В этом частном случае, погрешность (в процентах) произведения интервалов равна сумме погрешностей
  (в процентах) интервалов.

  В болеее общем случае, когда знаки l₁, u₁, l₂, u₂ заранее неизвестны (учитывая, что для интервала
  lᵢ ≤ uᵢ):

    A = [(c₁ - c₁p₁)(c₂ - c₂p₂), = [c₁c₂ - c₁c₂p₁ - c₁c₂p₂ + c₁c₂p₁p₂, ≈* [c₁c₂(1 - p₁ - p₂),
         (c₁ - c₁p₁)(c₂ + c₂p₂),    c₁c₂ - c₁c₂p₁ + c₁c₂p₂ - c₁c₂p₁p₂,     c₁c₂(1 - p₁ + p₂),
         (c₁ + c₁p₁)(c₂ - c₂p₂),    c₁c₂ + c₁c₂p₁ - c₁c₂p₂ - c₁c₂p₁p₂,     c₁c₂(1 + p₁ - p₂),
         (c₁ + c₁p₁)(c₂ + c₂p₂)]    c₁c₂ + c₁c₂p₁ + c₁c₂p₂ + c₁c₂p₁p₂]     c₁c₂(1 + p₁ + p₂)]

  * если рассматривать c₁c₂ как центр, то c₁c₂p₁p₂ можно пренебречь, т.к. p₁p₂ < 0.0001 и c₁c₂p₁p₂
  малая доля от величины интервала.

  Если c₁ и c₂ одного знака:

    w(i₁i₂) = w(min(A), max(A)) ≈ w(c₁c₂(1 - p₁ - p₂), c₁c₂(1 + p₁ + p₂)) = c₁c₂(p₁ + p₂)

  Если c₁ и c₂ противоположных знаков:

    w(i₁i₂) = w(min(A), max(A)) ≈ w(c₁c₂(1 + p₁ + p₂), c₁c₂(1 - p₁ - p₂)) = -c₁c₂(p₁ + p₂) =

    = |c₁c₂|(p₁ + p₂)

  Таким образом: p(i₁i₂) ≈ p(i₁) + p(i₂) ∎

  Проиллюстрируем это на примере:
|#

(#%require rackunit)

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (make-center-percent center width-percent)
  (let ((width (* (abs center) (/ width-percent 100))))
    (make-interval (- center width) (+ center width))))

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

(define (mul-interval a b)
  (let ((p1 (* (lower-bound a) (lower-bound b)))
        (p2 (* (lower-bound a) (upper-bound b)))
        (p3 (* (upper-bound a) (lower-bound b)))
        (p4 (* (upper-bound a) (upper-bound b))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define ac 10)
(define ap 0.09)

(define bc -5)
(define bp 0.09)

(define cc 1000000)
(define cp 0.09)

(define a (make-center-percent ac ap))
(define b (make-center-percent bc bp))
(define c (make-center-percent cc cp))

(check-equal? (< (abs (- (percent (mul-interval a b))
                         (+ (percent a) (percent b))))
                 0.0001) #t)

(check-equal? (< (abs (- (percent (mul-interval b c))
                         (+ (percent b) (percent c))))
                 0.0001) #t)

(check-equal? (< (abs (- (percent (mul-interval a c))
                         (+ (percent a) (percent c))))
                 0.0001) #t)
