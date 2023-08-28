#lang sicp

#|
  Упражнение 1.41

  Определите процедуру double, которая принимает как аргумент процедуру с одним аргументом и
  возвращает процедуру, которая применяет исходную процедуру дважды. Например, если процедура
  inc добавляет к своему аргументу 1, то (double inc) должна быть процедурой, которая добавляет 2.
  Скажите, какое значение возвращает:

    (((double (double double)) inc) 5)
|#

(#%require rackunit)

(define (double f)
  (lambda (x)
    (f (f x))))

(define (inc n) (+ n 1))

(define (square n) (* n n))

(check-equal? ((double inc) 1) 3)
(check-equal? ((double square) 2) 16)
(check-equal? (((double double) inc) 1) 5)
(check-equal? (((double double) square) 2) 65536)

#|
  Интуитивно хочестся ответить, что вызов процедуры (((double (double double)) inc) 5) вернёт
  13, потому что она double удваивает себя в (double double) и ещё раз в (double (double double)).
  Но в последнем случае, она применяет (double double) к самой себе. Т.е. создаёт процедуру вида

    ((double double) ((double double) inc))

  и вернёт 21. Это стоит проверить:
|#

(check-equal? (((double (double double)) inc) 10)
              (((double double) ((double double) inc)) 10))
(check-equal? (((double (double double)) inc) 5) 21)
