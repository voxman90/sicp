#lang sicp

#|
  Упражнение 2.2

  Рассмотрим задачу представления отрезков прямой на плоскости. Каждый отрезок представляется как
  пара точек: начало и конец. Определите конструктор make-segment и селекторы start-segment и
  end-segment, которые определяют представление отрезков в терминах точек. Далее, точку можно
  представить как пару чисел: координата x и координата y. Соответственно, напишите конструктор
  make-point и селекторы x-point и y-point, которые определяют такое представление. Наконец,
  используя свои селекторы и конструктор, напишите процедуру midpoint-segment, которая принимает
  отрезок в качестве аргумента и возвращает его середину (точку, координаты которой являются
  средним координат концов отрезка). Чтобы опробовать эти процедуры, Вам потребуется способ
  печатать координаты точек:

    (define (print-point p)
      (newline)
      (display "(")
      (display (x-point p))
      (display ",")
      (display (y-point p))
      (display ")"))
|#

(#%require rackunit)

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (add-point a b)
  (cons (+ (x-point a) (x-point b))
        (+ (y-point a) (y-point b))))

(define (div-point point div)
  (cons (/ (x-point point) div)
        (/ (y-point point) div)))

(define (mid-point a b)
  (div-point (add-point a b) 2))

(define (make-segment point-a point-b)
  (cons point-a point-b))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (mid-point (start-segment segment) (end-segment segment)))

(define p1 (make-point 2 3))
(define p2 (make-point 4 5))

(define s (make-segment p1 p2))

(check-equal? (make-segment p1 p2) '((2 . 3) 4 . 5))
(check-equal? (midpoint-segment s) '(3 . 4))
(check-equal? (start-segment s) p1)
(check-equal? (end-segment s) p2)
(check-equal? (x-point p1) 2)
(check-equal? (y-point p2) 5)
