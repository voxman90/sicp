#lang sicp

#|
  Упражнение 1.42

  Пусть f и g — две одноаргументные функции. По определению, композиция (composition) f и g есть
  функция x → f(g(x)). Определите процедуру compose которая реализует композицию. Например, если
  inc — процедура, добавляющая к своему аргументу 1.

    ((compose square inc) 6)
    49
|#

(#%require rackunit)

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (inc n)
  (+ n 1))

(define (square x)
  (* x x))

(define (double x)
  (* x 2))

(check-equal? ((compose square inc) 6) 49)
(check-equal? ((compose inc double) 6) 13)
(check-equal? ((compose double inc) 6) 14)
