#lang sicp

#|
  Упражнение 1.30

  Процедура sum порождает линейную рекурсию. Ее можно переписать так, чтобы суммирование выполнялось
  итеративно. Покажите, как сделать это, заполнив пропущенные выражения в следующем определении:

    (define (sum term a next b)
      (define (iter a result)
        (if <??>
            <??>
            (iter <??> <??>)))
      (iter <??> <??>))
|#

(#%require rackunit)

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (inc n) (+ n 1))

(define (cube x) (* x x x))

(define (identity x) x)

(check-equal? (sum cube 0 inc 2) 9)
(check-equal? (sum cube 1 inc 10) 3025)
(check-equal? (sum identity 1 inc 10) 55)
