#lang sicp

#|
  Упражнение 2.25

  Укажите комбинации car и cdr, которые извлекают 7 из следующих списков:

    (1 3 (5 7) 9)

    ((7))

    (1 (2 (3 (4 (5 (6 7))))))
|#

(#%require rackunit)

(define l1 (list 1 3 (list 5 7) 9))
(define l2 (list (list 7)))
(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(check-equal? (cadr (caddr l1)) 7)
(check-equal? (caar l2) 7)
(check-equal? (cadadr (cadadr (cadadr l3))) 7)
