#lang sicp

#|
  Упражнение 2.3

  Реализуйте представление прямоугольников на плоскости. (Подсказка: Вам могут потребоваться
  результаты упражнения 2.2). Определите в терминах своих конструкторов и селекторов процедуры,
  которые вычисляют периметр и площадь прямоугольника. Теперь реализуйте другое представление для
  прямоугольников. Можете ли Вы спроектировать свою систему с подходящими барьерами абстракции так,
  чтобы одни и те же процедуры вычисления периметра и площади работали с любым из Ваших представлений?
|#

(#%require rackunit)

(define (make-point x y)
  (cons x y))

(define (point-x point)
  (car point))

(define (point-y point)
  (cdr point))

(define (make-rectangle start-point width heigth)
  (let ((x (point-x start-point))
        (y (point-y start-point)))
    (cons (cons (+ x width) y)
          (cons x (+ y heigth)))))

(define (rectangle-width rectangle)
  (abs (- (point-x (car rectangle))
          (point-x (cdr rectangle)))))

(define (rectangle-heigth rectangle)
  (abs (- (point-y (cdr rectangle))
          (point-y (car rectangle)))))

(define (rectangle-square rectangle)
  (* (rectangle-width rectangle)
     (rectangle-heigth rectangle)))

(define (rectangle-perimeter rectangle)
  (* 2 (+ (rectangle-width rectangle)
          (rectangle-heigth rectangle))))

(define start-point (make-point 0 0))

(define rectangle (make-rectangle start-point 2 3))

(check-equal? (rectangle-square rectangle) 6)
(check-equal? (rectangle-perimeter rectangle) 10)
