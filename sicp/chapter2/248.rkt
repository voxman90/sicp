#lang sicp

#|
  Упражнение 2.48

  Направленный отрезок на плоскости можно представить в виде пары векторов: вектор от начала координат
  до начала отрезка и вектор от начала координат до конца отрезка. Используйте свое представление
  векторов из упражнения 2.46 и определите представление отрезков с конструктором make-segment и
  селекторами start-segment и end-segment.
|#

(#%require rackunit)

(define (make-vect x y)
  (cons x y))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define vect1 (make-vect 1.0 2.5))
(define vect2 (make-vect 0.0 5.0))

(define segment (make-segment vect1 vect2))

(check-equal? (start-segment segment) vect1)
(check-equal? (end-segment segment) vect2)
