#lang sicp

#|
  Упражнение 2.46

  Двумерный вектор v, идущий от начала координат к точке, можно представить в виде пары, состоящей из
  x-координаты и y-координаты. Реализуйте абстракцию данных для векторов, написав конструктор make-vect
  и соответствующие селекторы xcor-vect и ycor-vect. В терминах своих селекторов и конструктора
  реализуйте процедуры add-vect, sub-vect и scale-vect, которые выполняют операции сложения, вычитания
  векторов и умножения вектора на скаляр:

    (x₁, y₂) + (x₂, y₂) = (x₁ + x₂, y₁ + y₂)
    (x₁, y₁) − (x₂, y₂) = (x₁ − x₂, y₁ − y₂)
              s·(x, y) = (sx, sy)
|#

(#%require rackunit)

(define (make-vect x y)
  (cons x y))

(define (xcor-vect vector)
  (car vector))

(define (ycor-vect vector)
  (cdr vector))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect scalar vector)
  (make-vect (* scalar (xcor-vect vector))
             (* scalar (ycor-vect vector))))

(define vec1 (make-vect 1.0 2.0))
(define vec2 (make-vect 0.5 0.5))

(check-equal? (xcor-vect vec1) 1.0)
(check-equal? (ycor-vect vec2) 0.5)
(check-equal? (add-vect vec1 vec2) (make-vect 1.5 2.5))
(check-equal? (sub-vect vec1 vec2) (make-vect 0.5 1.5))
(check-equal? (scale-vect 2 vec2) (make-vect 1.0 1.0))
