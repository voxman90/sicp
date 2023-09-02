#lang sicp

#|
  Упражнение 2.21

  Процедура square-list принимает в качестве аргумента список чисел и возвращает список квадратов этих
  чисел.

    (square-list (list 1 2 3 4))
    (1 4 9 16)

  Перед Вами два различных определения square-list. Закончите их, вставив пропущенные выражения:

    (define (square-list items)
      (if (null? items)
          nil
          (cons <??> <??>)))

    (define (square-list2 items)
      (map <??> <??>))
|#

(#%require rackunit)

(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

(define test-list (list 3 5 7 8 21 4))

(check-equal? (square-list (list 1 2 3 4)) (list 1 4 9 16))
(check-equal? (square-list test-list) (square-list2 test-list))
