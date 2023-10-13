#lang racket

#|
  Упражнение 3.5

  Интегрирование методом Монте-Карло (Monte Carlo integration) — способ приближенного вычисления определенных
  интегралов при помощи моделирования методом Монте-Карло. Рассмотрим задачу вычисления площади фигуры,
  описываемой предикатом P(x, y), который истинен для точек (x, y), принадлежащих фигуре, и ложен для
  точек вне фигуры. Например, область, содержащаяся в круге с радиусом 3 и центром в точке (5, 7), описывается
  предикатом, проверяющим (x - 5)² + (y - 7)² ≤ 3². Чтобы оценить площадь фигуры, описываемой таким предикатом,
  для начала выберем прямоугольник, который содержит нашу фигуру. Например, прямоугольник с углами (2, 4) и
  (8, 10), расположенными по диагонали, содержит вышеописанный круг. Нужный нам интеграл — площадь той части
  прямоугольника, которая лежит внутри фигуры. Мы можем оценить интеграл, случайным образом выбирая точки (x,y),
  лежащие внутри прямоугольника, и проверяя для каждой точки P(x, y), чтобы определить, лежит ли точка внутри
  фигуры. Если мы проверим много точек, доля тех, которые окажутся внутри области, даст нам приближенное значение
  отношения площадей фигуры и прямоугольника. Таким образом, домножив это значение на площадь прямоугольника,
  мы получим приближенное значение интеграла.

  Реализуйте интегрирование методом Монте-Карло в виде процедуры estimate-integral, которая в качестве
  аргументов принимает предикат P, верхнюю и нижнюю границы прямоугольника x₁, x₂, y₁ и y₂, а также число
  проверок, которые мы должны осуществить, чтобы оценить отношение площадей. Ваша процедура должна использовать
  ту же самую процедуру monte-carlo, которая выше использовалась для оценки значения π. Оцените estimate-integral
  при помощи π измерив площадь единичного круга.

  Вам может пригодиться процедура, которая выдает число, случайно выбранное внутри данного отрезка.
  Нижеприведенная процедура random-in-range решает эту задачу, используя процедуру random, введенную в
  разделе 1.2.6, которая возвращает неотрицательное число меньше своего аргумента.

    (define (random-in-range low high)
      (let ((range (- high low)))
        (+ low (random range))))
|#

(#%require rackunit)

(define (sicp-random n)
  (if (and (exact? n) (integer? n))
      (random n)
      (* n (random))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))

  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (sicp-random range))))

(define (S x1 y1 x2 y2)
  (* (abs (- y2 y1))
     (abs (- x2 x1))))

(define (estimate-integral belongs-to-figure? x1 y1 x2 y2 trials-amount)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (belongs-to-figure? x y)))

  (* (monte-carlo trials-amount experiment)
     (S x1 y1 x2 y2)))

(define (make-square-pred x1 y1 x2 y2)
  (define (belongs-to-square? x y)
    (and (< x1 x) (< x x2)
         (< y1 y) (< y y2)))

  belongs-to-square?)

(define (make-circle-pred center-x center-y r)
  (define (square x) (* x x))

  (define (belongs-to-circle? x y)
    (< (+ (square (- x center-x))
          (square (- y center-y)))
       (square r)))

  belongs-to-circle?)

(define belongs-to-square1? (make-square-pred 0 0 0 0))
(define belongs-to-square2? (make-square-pred 0 0 1.0 1.0))
(define belongs-to-circle1? (make-circle-pred 0 0 1.0))

(check-equal? (estimate-integral belongs-to-square1? 0 0 1.0 1.0 100) 0)
(check-equal? (estimate-integral belongs-to-square2? 0 0 1.0 1.0 100) 1.0)

(define estimation-result (estimate-integral belongs-to-circle1? -1.0 -1.0 1.0 1.0 10000))

(check-true (and (< 3.1 estimation-result) (< estimation-result 3.2)))
