#lang sicp

#|
  Упражнение 2.17

  Определите процедуру last-pair, которая возвращает список, содержащий только последний элемент
  данного (непустого) списка.

    (last-pair (list 23 72 149 34))
    (34)
|#

(#%require rackunit)

(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

(check-equal? (last-pair (list 1 2 3 4)) 4)
(check-equal? (last-pair '(4 8 12 16 1)) 1)
(check-equal? (last-pair (list 23 72 149 34)) 34)
