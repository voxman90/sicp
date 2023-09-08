#lang sicp

#|
  Упражнение 2.54

  Предикат equal? для двух списков возвращает истину, если они содержат одни и те же элементы в
  одинаковом порядке. Например,

    (equal? '(this is a list) '(this is a list))

  истинно, но

    (equal? '(this is a list) '(this (is a) list))

  ложно. Более точно, можно определить equal? рекурсивно в терминах базового равенства символов eq?,
  сказав, что a равно b, если оба они символы и для них выполняется eq? либо оба они списки и при этом
  верно, что (car a) равняется в смысле equal? (car b), а (cdr a) равняется в смысле equal? (cdr b).
  Пользуясь этой идеей, напишите equal? в виде процедуры equal-proc?.
|#

(#%require rackunit)

(define (equal-proc? a b)
  (if (and (pair? a) (pair? b))
      (and (equal-proc? (car a) (car b))
           (equal-proc? (cdr a) (cdr b)))
      (eq? a b)))

(check-equal? (equal? '(this is a list) '(this is a list)) #t)
(check-equal? (equal? '(this is a list) '(this (is a) list)) #f)
(check-equal? (equal? '(this (is a) list) '(this (is a) list)) #t)
(check-equal? (equal? '(this (is a)) '(this (is a) list)) #f)
(check-equal? (equal? '(this (is (a) (list !))) '(this (is (a) (list !)))) #t)

(check-equal? (equal-proc? '(this is a list) '(this is a list)) #t)
(check-equal? (equal-proc? '(this is a list) '(this (is a) list)) #f)
(check-equal? (equal-proc? '(this (is a) list) '(this (is a) list)) #t)
(check-equal? (equal-proc? '(this (is (a) (list !))) '(this (is (a) (list !)))) #t)
