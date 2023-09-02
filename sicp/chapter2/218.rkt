#lang sicp

#|
  Упражнение 2.18

  Определите процедуру reverse, которая принимает список как аргумент и возвращает список, состоящий
  из тех же элементов в обратном порядке:

    (reverse (list 1 4 9 16 25))
    (25 16 9 4 1)
|#

(#%require rackunit)

(define (reverse l)
  (define (iter l reverted-l)
    (if (null? l)
      reverted-l
      (iter (cdr l) (cons (car l) reverted-l))))

  (iter l '()))

(check-equal? (reverse '()) '())
(check-equal? (reverse (list 1)) (list 1))
(check-equal? (reverse (list 2 4 6 9)) (list 9 6 4 2))
(check-equal? (reverse (list 1 4 9 16 25)) '(25 16 9 4 1))
