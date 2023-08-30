#lang sicp

#|
  Упражнение 2.4

  Вот еще одно процедурное представление для пар.
  Проверьте для этого представления, что (car(cons x y)) возвращает x при любых двух объектах x и y.

    (define (cons x y)
      (lambda (m) (m x y)))

    (define (car z)
      (z (lambda (p q) p)))

  Каково соответствующее определение cdr? (Подсказка: Чтобы проверить, что это работает, используйте
  подстановочную модель из раздела 1.1.5.)
|#

#|
  (define pair (cons x y))
  (define pair (lambda (m) (m x y)))

  (car pair)
  (car (lambda (m) (m x y))))
  ((lambda (m) (m x y))) (lambda (p q) p))
  ((lambda ((lambda (p q) p)))))
  ((lambda (p q) p) x y)
  (lambda (x y) p)
  x
|#

(#%require rackunit)

(define (cons x y)
  (lambda (m) (m x y)))

(define (car pair)
  (pair (lambda (p _) p)))

(define (cdr pair)
  (pair (lambda (_ q) q)))

(define example (cons 1 2))

(check-equal? (car example) 1)
(check-equal? (cdr example) 2)
(check-equal? (cdr (cons 5 15)) 15)
