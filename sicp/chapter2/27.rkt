#lang sicp

#|
  Упражнение 2.7

  Программа Лизы неполна, поскольку она не определила, как реализуется абстракция интервала. Вот
  определение конструктора интервала:

    (define (make-interval a b) (cons a b))

  Завершите реализацию, определив селекторы upper-bound и lower-bound.
|#

(#%require rackunit)

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define lower 5)

(define upper 10)

(define interval (make-interval lower upper))

(check-equal? (lower-bound interval) lower)
(check-equal? (upper-bound interval) upper)
